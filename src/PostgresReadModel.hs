{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module PostgresReadModel where

import           Contravariant.Extras.Contrazip
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8          as C
import qualified Data.ByteString.Lazy           as BL
import           Data.Either.Combinators        (maybeToRight)
import           Data.Foldable
import           Data.Functor.Contravariant
import           Data.Maybe                     (fromMaybe)
import           Data.Time.Clock                (UTCTime)
import qualified Hasql.Connection
import qualified Hasql.Decoders                 as D
import qualified Hasql.Encoders                 as E
import           Hasql.Session                  as Session hiding (sql)
import           Hasql.Statement                (Statement (..))
import qualified Hasql.Transaction              as Tx
import           Hasql.Transaction.Sessions
import           ReadModel
import           Types

newPostgresReadModel :: Hasql.Connection.Connection -> ReadModel
newPostgresReadModel connection = ReadModel { handle = handleEvent connection }

handleEvent :: Hasql.Connection.Connection -> OffsetEvent -> IO ()
handleEvent connection event =
  case payload event of
    GameCreated streamId time clientId color -> insertChallenge connection (offset event) streamId clientId color time
    GameJoined streamId time clientId -> gameJoined connection (offset event) streamId clientId time
    YellowPlayed streamId time column -> played connection (offset event) streamId column time
    RedPlayed streamId time column -> played connection (offset event) streamId column time
    GameWon streamId time clientId -> gameWon connection (offset event) streamId clientId time
    GameTied streamId time -> draw connection (offset event) streamId time

---- ENCODER ----

colorEncoder :: E.Value Color
colorEncoder = contramap colorText E.text
  where
    colorText color = case color of
      Red    -> "red"
      Yellow -> "yellow"

gameStateEncoder :: E.Value GameState
gameStateEncoder = contramap gameStateText E.text
  where
    gameStateText gameState = case gameState of
      InProgress -> "in_progress"
      YellowWon  -> "yellow_won"
      RedWon     -> "red_won"
      Draw       -> "draw"

streamIdEncoder :: E.Value StreamId
streamIdEncoder = contramap (\(StreamId streamId) -> streamId) E.uuid

clientIdEncoder :: E.Value ClientId
clientIdEncoder = contramap (\(ClientId clientId) -> clientId) E.uuid

offsetEncoder :: E.Value Offset
offsetEncoder = contramap (\(Offset version) -> fromIntegral version) E.int8

columnEncoder :: E.Value Column
columnEncoder = contramap (\(Column column) -> fromIntegral column) E.int4

movesEncoder :: E.Value [Column]
movesEncoder = E.array (E.dimension foldl' (E.element (E.nonNullable columnEncoder)))

---- DECODER ----

colorDecoder :: D.Value Color
colorDecoder = D.custom toColor
  where
    toColor _ str =
      case str of
        "red"    -> Right Red
        "yellow" -> Right Yellow
        _        -> Left "invalid value for color"

movesDecoder :: D.Value [Column]
movesDecoder = D.array (D.dimension replicateM (D.element (D.nonNullable (Column <$> D.int4))))

eventDecoder :: D.Value Event
eventDecoder =
  D.custom (\_ str -> maybeToRight "invalid event format" (decode (BL.fromStrict str)))

---- DELETE ----

deleteStatement :: C.ByteString -> Statement () ()
deleteStatement tableName = Statement sql enc dec True
  where
    sql = "DELETE FROM " <> tableName
    enc = E.noParams
    dec = D.noResult

delete :: Hasql.Connection.Connection -> C.ByteString -> IO (Either QueryError ())
delete connection tableName = run (statement () (deleteStatement tableName)) connection

---- OFFSET ----

selectOffsetStatement :: Statement () (Maybe Offset)
selectOffsetStatement = Statement sql encoder decoder True
  where
    sql = "SELECT latest FROM offsets LIMIT 1"
    encoder = E.noParams
    decoder = D.rowMaybe $ D.column (D.nonNullable (Offset <$> D.int8))

updateOffsetStatement :: Statement Offset ()
updateOffsetStatement = Statement sql encoder decoder True
  where
    sql = "UPDATE offsets SET latest=$1"
    encoder = E.param (E.nonNullable offsetEncoder)
    decoder = D.noResult

gt :: Offset -> Offset -> Bool
gt (Offset lhs) (Offset rhs) = lhs > rhs

---- GAME TIED ----

updateGameTiedStatement :: Statement (StreamId, UTCTime) ()
updateGameTiedStatement = Statement sql encoder decoder True
  where
    sql = "UPDATE games_internal SET game_state='draw', timestamp=$2 WHERE id=$1"
    encoder = contrazip2
     (E.param (E.nonNullable streamIdEncoder))
     (E.param (E.nonNullable E.timestamptz))
    decoder = D.noResult

updateGameTiedTransaction :: Offset -> StreamId -> UTCTime -> Tx.Transaction ()
updateGameTiedTransaction eventOffset streamId time = do
  maybeLatestOffset <- Tx.statement () selectOffsetStatement
  let latestOffset = fromMaybe (Offset (-1)) maybeLatestOffset
  when (gt eventOffset latestOffset) $ do
    Tx.statement (streamId, time) updateGameTiedStatement
    Tx.statement eventOffset updateOffsetStatement

updateGameTiedSession :: Offset -> StreamId -> UTCTime -> Session ()
updateGameTiedSession version streamId time =
  transaction Serializable Write (updateGameTiedTransaction version streamId time)

draw :: Hasql.Connection.Connection -> Offset -> StreamId -> UTCTime -> IO ()
draw connection version streamId time = do
  dbResult <- run (updateGameTiedSession version streamId time) connection
  crashOnError dbResult

---- GAME WON ----

selectPlayersStatement :: Statement StreamId (ClientId, ClientId)
selectPlayersStatement = Statement sql encoder decoder True
  where
    sql = "SELECT player_red, player_yellow FROM games_internal WHERE id=$1"
    encoder = E.param (E.nonNullable streamIdEncoder)
    decoder =
      D.singleRow $
        (,) <$> D.column (D.nonNullable (ClientId <$> D.uuid))
          <*> D.column (D.nonNullable (ClientId <$> D.uuid))

updateGameWonStatement :: Statement (StreamId, GameState, UTCTime) ()
updateGameWonStatement = Statement sql encoder decoder True
  where
    sql = "UPDATE games_internal SET game_state=$2, timestamp=$3 WHERE id=$1"
    encoder =
      contrazip3
        (E.param (E.nonNullable streamIdEncoder))
        (E.param (E.nonNullable gameStateEncoder))
        (E.param (E.nonNullable E.timestamptz))
    decoder = D.noResult

updateGameWonTransaction :: Offset -> StreamId -> ClientId -> UTCTime -> Tx.Transaction ()
updateGameWonTransaction eventOffset streamId clientId time = do
  maybeLatestOffset <- Tx.statement () selectOffsetStatement
  let latestOffset = fromMaybe (Offset (-1)) maybeLatestOffset
  when (gt eventOffset latestOffset) $ do
    (playerRed, playerYellow) <- Tx.statement streamId selectPlayersStatement
    if playerRed == clientId
      then Tx.statement (streamId, RedWon, time) updateGameWonStatement
      else when (playerYellow == clientId) $ Tx.statement (streamId, YellowWon, time) updateGameWonStatement
    Tx.statement eventOffset updateOffsetStatement

updateGameWonSession :: Offset -> StreamId -> ClientId -> UTCTime -> Session ()
updateGameWonSession eventOffset streamId clientId time =
  transaction Serializable Write (updateGameWonTransaction eventOffset streamId clientId time)

gameWon :: Hasql.Connection.Connection -> Offset -> StreamId -> ClientId -> UTCTime -> IO ()
gameWon connection eventOffset streamId clientId time = do
  dbResult <- run (updateGameWonSession eventOffset streamId clientId time) connection
  crashOnError dbResult

---- YELLOW/RED PLAYED ----

selectMovesStatement :: Statement StreamId [Column]
selectMovesStatement = Statement sql encoder decoder True
  where
    sql = "SELECT moves FROM games_internal WHERE id=$1"
    encoder = E.param (E.nonNullable streamIdEncoder)
    decoder = D.singleRow $ D.column (D.nonNullable movesDecoder)

updateGameStatement :: Statement ([Column], StreamId, UTCTime) ()
updateGameStatement = Statement sql encoder decoder True
  where
    sql = "UPDATE games_internal SET moves=$1, timestamp=$3 WHERE id=$2"
    encoder =
      contrazip3
        (E.param (E.nonNullable movesEncoder))
        (E.param (E.nonNullable streamIdEncoder))
        (E.param (E.nonNullable E.timestamptz))
    decoder = D.noResult

updateGameTransaction :: Offset -> StreamId -> Column -> UTCTime -> Tx.Transaction ()
updateGameTransaction eventOffset streamId column time = do
  maybeLatestOffset <- Tx.statement () selectOffsetStatement
  let latestOffset = fromMaybe (Offset (-1)) maybeLatestOffset
  when (gt eventOffset latestOffset) $ do
    moves <- Tx.statement streamId selectMovesStatement
    Tx.statement (moves ++ [column], streamId, time) updateGameStatement
    Tx.statement eventOffset updateOffsetStatement

updateGameSession :: Offset -> StreamId -> Column -> UTCTime -> Session ()
updateGameSession eventOffset streamId column time =
  transaction Serializable Write (updateGameTransaction eventOffset streamId column time)

played :: Hasql.Connection.Connection -> Offset -> StreamId -> Column -> UTCTime -> IO ()
played connection eventOffset streamId column time = do
  dbResult <- run (updateGameSession eventOffset streamId column time) connection
  crashOnError dbResult

---- GAME CREATED ----

insertChallengeStatement :: Statement (StreamId, ClientId, Color, UTCTime) ()
insertChallengeStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO challenges_internal (id, client_id, color, timestamp) VALUES ($1, $2, $3, $4) ON CONFLICT (id) DO NOTHING"
    encoder =
      contrazip4
        (E.param (E.nonNullable streamIdEncoder))
        (E.param (E.nonNullable clientIdEncoder))
        (E.param (E.nonNullable colorEncoder))
        (E.param (E.nonNullable E.timestamptz))
    decoder = D.noResult

insertChallengeTransaction :: Offset -> StreamId -> ClientId -> Color -> UTCTime -> Tx.Transaction ()
insertChallengeTransaction eventOffset streamId clientId color time = do
  maybeLatestOffset <- Tx.statement () selectOffsetStatement
  let latestOffset = fromMaybe (Offset (-1)) maybeLatestOffset
  when (gt eventOffset latestOffset) $ do
    Tx.statement (streamId, clientId, color, time) insertChallengeStatement
    Tx.statement eventOffset updateOffsetStatement

insertChallengeSession :: Offset -> StreamId -> ClientId -> Color -> UTCTime -> Session ()
insertChallengeSession eventOffset streamId clientId color time =
  transaction Serializable Write (insertChallengeTransaction eventOffset streamId clientId color time)

insertChallenge :: Hasql.Connection.Connection -> Offset -> StreamId -> ClientId -> Color -> UTCTime -> IO ()
insertChallenge conn eventOffset streamId clientId color time = do
  dbResult <- run (insertChallengeSession eventOffset streamId clientId color time) conn
  crashOnError dbResult

---- GAME JOINED ----

selectChallengeStatement :: Statement StreamId (ClientId, Color)
selectChallengeStatement = Statement sql encoder decoder True
  where
    sql = "SELECT client_id, color FROM challenges_internal WHERE id=$1"
    encoder = E.param (E.nonNullable streamIdEncoder)
    decoder = D.singleRow ((,) <$> D.column (D.nonNullable (ClientId <$> D.uuid)) <*> D.column (D.nonNullable colorDecoder))

insertGameStatement :: Statement (StreamId, GameState, [Column], ClientId, ClientId, UTCTime) ()
insertGameStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO games_internal (id, game_state, moves, player_red, player_yellow, timestamp) VALUES ($1, $2, $3, $4, $5, $6)"
    encoder =
      contrazip6
        (E.param (E.nonNullable streamIdEncoder))
        (E.param (E.nonNullable gameStateEncoder))
        (E.param (E.nonNullable movesEncoder))
        (E.param (E.nonNullable clientIdEncoder))
        (E.param (E.nonNullable clientIdEncoder))
        (E.param (E.nonNullable E.timestamptz))
    decoder = D.noResult

deleteChallengeStatement :: Statement StreamId ()
deleteChallengeStatement = Statement sql encoder decoder True
  where
    sql = "DELETE FROM challenges_internal WHERE id=$1"
    encoder = E.param (E.nonNullable streamIdEncoder)
    decoder = D.noResult

insertGameTransaction :: Offset -> StreamId -> ClientId -> UTCTime -> Tx.Transaction ()
insertGameTransaction eventOffset streamId clientId time = do
  maybeLatestOffset <- Tx.statement () selectOffsetStatement
  let latestOffset = fromMaybe (Offset (-1)) maybeLatestOffset
  when (gt eventOffset latestOffset) $ do
    (opponentId, opponentColor) <- Tx.statement streamId selectChallengeStatement
    let params = case opponentColor of
          Red    -> (streamId, InProgress, [], opponentId, clientId, time)
          Yellow -> (streamId, InProgress, [], clientId, opponentId, time)
    Tx.statement params insertGameStatement
    Tx.statement streamId deleteChallengeStatement
    Tx.statement eventOffset updateOffsetStatement

insertGameSession :: Offset -> StreamId -> ClientId -> UTCTime -> Session ()
insertGameSession eventOffset streamId clientId time =
  transaction Serializable Write (insertGameTransaction eventOffset streamId clientId time)

gameJoined :: Hasql.Connection.Connection -> Offset -> StreamId -> ClientId -> UTCTime -> IO ()
gameJoined connection eventOffset streamId clientId time = do
  dbResult <- run (insertGameSession eventOffset streamId clientId time) connection
  crashOnError dbResult

crashOnError :: Show e => Either e a -> IO ()
crashOnError (Left err) = error $ show err
crashOnError (Right _)  = pure ()

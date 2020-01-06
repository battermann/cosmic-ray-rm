{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Contravariant.Extras.Contrazip
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.Functor.Contravariant
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, connectPostgreSQL, execute_, postgreSQLConnectionString)
import Database.PostgreSQL.Simple.Notification (getNotification, notificationData)
import qualified Hasql.Connection
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Session as Session hiding (sql)
import Hasql.Statement (Statement (..))
import qualified Hasql.Transaction as Tx
import Hasql.Transaction.Sessions
import System.Environment
import System.IO (hFlush, stdout)
import Types

type ReadModelConnection = Hasql.Connection.Connection

type EventStoreConnection = Database.PostgreSQL.Simple.Connection

main :: IO ()
main = do
  readModelEnv <- lookupEnv "READ_MODEL"
  eventStoreEnv <- lookupEnv "EVENT_STORE"
  let rmConnStr = maybe defaultRmConnStr C.pack readModelEnv
  let esConnStr = maybe defaultEsConnStr C.pack eventStoreEnv
  rmConnOrError <- Hasql.Connection.acquire rmConnStr
  esConn <- connectPostgreSQL esConnStr
  void $ listenAndLoop esConn `traverse` rmConnOrError
  where
    defaultRmConnStr = Hasql.Connection.settings "localhost" 15432 "postgres" "secret" "postgres"
    defaultEsConnStr = postgreSQLConnectionString $ ConnectInfo "localhost" 5432 "postgres" "secret" "postgres"

listenAndLoop :: EventStoreConnection -> ReadModelConnection -> IO ()
listenAndLoop esConn rmConn = do
  void $ execute_ esConn "LISTEN events"
  loop esConn rmConn

loop :: EventStoreConnection -> ReadModelConnection -> IO ()
loop connection rmConnection = do
  hFlush stdout
  notification <- getNotification connection
  let maybeEvent = decode ((BL.fromStrict . notificationData) notification) :: Maybe VersionedEvent
  void $ case maybeEvent of
    Just event -> do
      putStrLn $ "Received event\n" <> show event
      handle rmConnection event
    Nothing -> putStrLn $ "Could not decode msg\n" <> show notification
  loop connection rmConnection

handle :: ReadModelConnection -> VersionedEvent -> IO ()
handle connection (VersionedEvent version event) =
  case event of
    GameCreated streamId clientId color -> insertChallenge connection streamId clientId color
    GameJoined streamId clientId -> gameJoined connection streamId clientId
    YellowPlayed streamId column -> played connection version streamId column
    RedPlayed streamId column -> played connection version streamId column
    GameWon streamId clientId -> gameWon connection version streamId clientId
    GameTied streamId -> draw connection version streamId

---- ENCODER ----

colorEncoder :: E.Value Color
colorEncoder = contramap colorText E.text
  where
    colorText color = case color of
      Red -> "red"
      Yellow -> "yellow"

gameStateEncoder :: E.Value GameState
gameStateEncoder = contramap gameStateText E.text
  where
    gameStateText gameState = case gameState of
      InProgress -> "in_progress"
      YellowWon -> "yellow_won"
      RedWon -> "red_won"
      Draw -> "draw"

streamIdEncoder :: E.Value StreamId
streamIdEncoder = contramap (\(StreamId streamId) -> streamId) E.uuid

clientIdEncoder :: E.Value ClientId
clientIdEncoder = contramap (\(ClientId clientId) -> clientId) E.uuid

versionEncoder :: E.Value Version
versionEncoder = contramap (\(Version version) -> fromIntegral version) E.int4

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
        "red" -> Right Red
        "yellow" -> Right Yellow
        _ -> Left "invalid value for color"

movesDecoder :: D.Value [Column]
movesDecoder = D.array (D.dimension replicateM (D.element (D.nonNullable (Column <$> D.int4))))

---- GAME TIED ----

updateGameTiedStatement :: Statement (Version, StreamId) ()
updateGameTiedStatement = Statement sql encoder decoder True
  where
    sql = "UPDATE games_internal SET version=$1, game_state='draw' WHERE id=$2"
    encoder =
      contrazip2
        (E.param (E.nonNullable versionEncoder))
        (E.param (E.nonNullable streamIdEncoder))
    decoder = D.noResult

updateGameTiedTransaction :: Version -> StreamId -> Tx.Transaction ()
updateGameTiedTransaction version streamId = do
  (previousVersion, _) <- Tx.statement streamId selectMovesStatement
  when (isInc1 previousVersion version) $ Tx.statement (version, streamId) updateGameTiedStatement
  where
    isInc1 :: Version -> Version -> Bool
    isInc1 (Version previous) (Version next) = next == previous + 1

updateGameTiedSession :: Version -> StreamId -> Session ()
updateGameTiedSession version streamId =
  transaction Serializable Write (updateGameTiedTransaction version streamId)

draw :: ReadModelConnection -> Version -> StreamId -> IO ()
draw connection version streamId = do
  dbResult <- run (updateGameTiedSession version streamId) connection
  logOnError dbResult

---- GAME WON ----

selectPlayersStatement :: Statement StreamId (Version, ClientId, ClientId)
selectPlayersStatement = Statement sql encoder decoder True
  where
    sql = "SELECT version, player_red, player_yellow FROM games_internal WHERE id=$1"
    encoder = E.param (E.nonNullable streamIdEncoder)
    decoder =
      D.singleRow
        ( (,,) <$> D.column (D.nonNullable (Version <$> D.int4))
            <*> D.column (D.nonNullable (ClientId <$> D.uuid))
            <*> D.column (D.nonNullable (ClientId <$> D.uuid))
        )

updateGameWonStatement :: Statement (Version, StreamId, GameState) ()
updateGameWonStatement = Statement sql encoder decoder True
  where
    sql = "UPDATE games_internal SET version=$1, game_state=$3 WHERE id=$2"
    encoder =
      contrazip3
        (E.param (E.nonNullable versionEncoder))
        (E.param (E.nonNullable streamIdEncoder))
        (E.param (E.nonNullable gameStateEncoder))
    decoder = D.noResult

updateGameWonTransaction :: Version -> StreamId -> ClientId -> Tx.Transaction ()
updateGameWonTransaction version streamId clientId = do
  (previousVersion, playerRed, playerYellow) <- Tx.statement streamId selectPlayersStatement
  when (isInc1 previousVersion version) $
    if playerRed == clientId
      then Tx.statement (version, streamId, RedWon) updateGameWonStatement
      else
        when (playerYellow == clientId) $
          Tx.statement (version, streamId, YellowWon) updateGameWonStatement
  where
    isInc1 :: Version -> Version -> Bool
    isInc1 (Version previous) (Version next) = next == previous + 1

updateGameWonSession :: Version -> StreamId -> ClientId -> Session ()
updateGameWonSession version streamId clientId =
  transaction Serializable Write (updateGameWonTransaction version streamId clientId)

gameWon :: ReadModelConnection -> Version -> StreamId -> ClientId -> IO ()
gameWon connection version streamId clientId = do
  dbResult <- run (updateGameWonSession version streamId clientId) connection
  logOnError dbResult

---- YELLOW/RED PLAYED ----

selectMovesStatement :: Statement StreamId (Version, [Column])
selectMovesStatement = Statement sql encoder decoder True
  where
    sql = "SELECT version, moves FROM games_internal WHERE id=$1"
    encoder = E.param (E.nonNullable streamIdEncoder)
    decoder = D.singleRow ((,) <$> D.column (D.nonNullable (Version <$> D.int4)) <*> D.column (D.nonNullable movesDecoder))

updateGameStatement :: Statement ([Column], Version, StreamId) ()
updateGameStatement = Statement sql encoder decoder True
  where
    sql = "UPDATE games_internal SET moves=$1, version=$2 WHERE id=$3"
    encoder =
      contrazip3
        (E.param (E.nonNullable movesEncoder))
        (E.param (E.nonNullable versionEncoder))
        (E.param (E.nonNullable streamIdEncoder))
    decoder = D.noResult

updateGameTransaction :: Version -> StreamId -> Column -> Tx.Transaction ()
updateGameTransaction version streamId column = do
  (previousVersion, moves) <- Tx.statement streamId selectMovesStatement
  when (isInc1 previousVersion version) $ Tx.statement (moves ++ [column], version, streamId) updateGameStatement
  where
    isInc1 :: Version -> Version -> Bool
    isInc1 (Version previous) (Version next) = next == previous + 1

updateGameSession :: Version -> StreamId -> Column -> Session ()
updateGameSession version streamId column =
  transaction Serializable Write (updateGameTransaction version streamId column)

played :: ReadModelConnection -> Version -> StreamId -> Column -> IO ()
played connection version streamId column = do
  dbResult <- run (updateGameSession version streamId column) connection
  logOnError dbResult

---- GAME CREATED ----

insertChallengeStatement :: Statement (StreamId, ClientId, Color) ()
insertChallengeStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO challenges_internal (id, client_id, color) VALUES ($1, $2, $3) ON CONFLICT (id) DO NOTHING"
    encoder =
      contrazip3
        (E.param (E.nonNullable streamIdEncoder))
        (E.param (E.nonNullable clientIdEncoder))
        (E.param (E.nonNullable colorEncoder))
    decoder = D.noResult

insertChallengeSession :: StreamId -> ClientId -> Color -> Session ()
insertChallengeSession streamId clientId color = Session.statement (streamId, clientId, color) insertChallengeStatement

insertChallenge :: ReadModelConnection -> StreamId -> ClientId -> Color -> IO ()
insertChallenge conn streamId clientId color = do
  dbResult <- run (insertChallengeSession streamId clientId color) conn
  logOnError dbResult

---- GAME JOINED ----

selectChallengeStatement :: Statement StreamId (ClientId, Color)
selectChallengeStatement = Statement sql encoder decoder True
  where
    sql = "SELECT client_id, color FROM challenges_internal WHERE id=$1"
    encoder = E.param (E.nonNullable streamIdEncoder)
    decoder = D.singleRow ((,) <$> D.column (D.nonNullable (ClientId <$> D.uuid)) <*> D.column (D.nonNullable colorDecoder))

insertGameStatement :: Statement (StreamId, Version, GameState, [Column], ClientId, ClientId) ()
insertGameStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO games_internal (id, version, game_state, moves, player_red, player_yellow) VALUES ($1, $2, $3, $4, $5, $6)"
    encoder =
      contrazip6
        (E.param (E.nonNullable streamIdEncoder))
        (E.param (E.nonNullable versionEncoder))
        (E.param (E.nonNullable gameStateEncoder))
        (E.param (E.nonNullable movesEncoder))
        (E.param (E.nonNullable clientIdEncoder))
        (E.param (E.nonNullable clientIdEncoder))
    decoder = D.noResult

deleteChallengeStatement :: Statement StreamId ()
deleteChallengeStatement = Statement sql encoder decoder True
  where
    sql = "DELETE FROM challenges_internal WHERE id=$1"
    encoder = E.param (E.nonNullable streamIdEncoder)
    decoder = D.noResult

insertGameTransaction :: StreamId -> ClientId -> Tx.Transaction ()
insertGameTransaction streamId clientId = do
  (opponentId, opponentColor) <- Tx.statement streamId selectChallengeStatement
  let params = case opponentColor of
        Red -> (streamId, Version 1, InProgress, [], opponentId, clientId)
        Yellow -> (streamId, Version 1, InProgress, [], clientId, opponentId)
  Tx.statement params insertGameStatement
  Tx.statement streamId deleteChallengeStatement

insertGameSession :: StreamId -> ClientId -> Session ()
insertGameSession streamId clientId =
  transaction Serializable Write (insertGameTransaction streamId clientId)

gameJoined :: ReadModelConnection -> StreamId -> ClientId -> IO ()
gameJoined connection streamId clientId = do
  dbResult <- run (insertGameSession streamId clientId) connection
  logOnError dbResult

logOnError :: Show e => Either e a -> IO ()
logOnError (Left err) = print err
logOnError (Right _) = pure ()

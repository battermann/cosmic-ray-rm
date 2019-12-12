{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Contravariant.Extras.Contrazip
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.Functor.Contravariant
import Data.Int
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
  result <- listenAndLoop esConn `traverse` rmConnOrError
  case result of
    Left err -> print err
    Right () -> return ()
  undefined
  where
    defaultRmConnStr = Hasql.Connection.settings "localhost" 15432 "postgres" "secret" "postgres"
    defaultEsConnStr = postgreSQLConnectionString $ ConnectInfo "localhost" 5432 "postgres" "secret" "postgres"

listenAndLoop :: EventStoreConnection -> ReadModelConnection -> IO ()
listenAndLoop esConn rmConn = do
  _ <- execute_ esConn "LISTEN events"
  loop esConn rmConn

loop :: EventStoreConnection -> ReadModelConnection -> IO ()
loop connection rmConnection = do
  notification <- getNotification connection
  let maybeEvent = decode ((BL.fromStrict . notificationData) notification) :: Maybe VersionedEvent
  _ <- case maybeEvent of
    Just event -> handle rmConnection event
    Nothing -> putStrLn "Could not decode msg"
  loop connection rmConnection

handle :: ReadModelConnection -> VersionedEvent -> IO ()
handle connection (VersionedEvent _ event) =
  case event of
    GameCreated streamId clientId color -> insertChallenge connection streamId clientId color
    GameJoined streamId clientId -> gameJoined connection streamId clientId
    YellowPlayed _ _ -> undefined
    RedPlayed _ _ -> undefined
    GameWon _ _ -> undefined
    GameTied _ -> undefined

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

streamIdEncoder :: E.Value StreamId
streamIdEncoder = contramap (\(StreamId streamId) -> streamId) E.uuid

clientIdEncoder :: E.Value ClientId
clientIdEncoder = contramap (\(ClientId clientId) -> clientId) E.uuid

versionEncoder :: E.Value Version
versionEncoder = contramap (\(Version version) -> fromIntegral version) E.int4

---- DECODER ----

colorDecoder :: D.Value Color
colorDecoder = D.custom toColor
  where
    toColor _ str =
      case str of
        "red" -> Right Red
        "yellow" -> Right Yellow
        _ -> Left "invalid value for color"

---- GAME CREATED ----

insertChallengeStatement :: Statement (StreamId, ClientId, Color) ()
insertChallengeStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO challenges_internal (id, client_id, color) VALUES ($1, $2, $3)"
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
  case dbResult of
    Left err -> error (show err)
    Right _ -> return ()

---- GAME JOINED ----

selectChallengeStatement :: Statement StreamId (ClientId, Color)
selectChallengeStatement = Statement sql encoder decoder True
  where
    sql = "SELECT client_id, color FROM challenges_internal WHERE id=$1"
    encoder = E.param (E.nonNullable streamIdEncoder)
    decoder = D.singleRow ((,) <$> D.column (D.nonNullable (ClientId <$> D.uuid)) <*> D.column (D.nonNullable colorDecoder))

insertGameStatement :: Statement (StreamId, Version, GameState, [Int32], ClientId, ClientId) ()
insertGameStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO games_internal (id, version, game_state, moves, player_red, player_yellow) VALUES ($1, $2, $3, $4, $5, $6)"
    encoder =
      contrazip6
        (E.param (E.nonNullable streamIdEncoder))
        (E.param (E.nonNullable versionEncoder))
        (E.param (E.nonNullable gameStateEncoder))
        (E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable E.int4))))))
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
  case dbResult of
    Right _ -> return ()
    Left err -> error $ show err

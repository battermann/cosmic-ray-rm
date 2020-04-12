{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent    (threadDelay)
import qualified Data.ByteString.Char8 as C
import           Data.Foldable         (traverse_)
import           Data.Maybe            (fromMaybe)
import           Database.Redis        (checkedConnect, parseConnectInfo)
import           EventStream
import qualified Hasql.Connection
import           PostgresReadModel     (newPostgresReadModel)
import           ReadModel
import           RedisEventStream
import           System.Environment    (lookupEnv)
import           Types

main :: IO ()
main = do
  redisConnectionString <- fromMaybe "redis://localhost:6379" <$> lookupEnv "REDIS_URL"
  redisConn <- case parseConnectInfo redisConnectionString of
                Left err   -> error err
                Right info -> checkedConnect info
  readModelEnv <- lookupEnv "READ_MODEL"
  let readModelSettings = maybe defaultSettings C.pack readModelEnv
  readModelConnOrError <- Hasql.Connection.acquire readModelSettings
  let readModel = case readModelConnOrError of
                    Left err   -> error $ show err
                    Right conn -> newPostgresReadModel conn
  eventStream <- newRedisEventStreamWithRetries redisConn (Stream "events") (Group "connect-4") (Consumer "consumer-1")
  loop eventStream readModel
  where
    defaultSettings = Hasql.Connection.settings "localhost" 15432 "postgres" "secret" "postgres"

loop :: EventStream MessageId -> ReadModel -> IO ()
loop eventStream readModel = do
  events <- query eventStream
  print `traverse_` events
  (handleAndAck eventStream readModel) `traverse_` events
  threadDelay (250 * 1000) -- wait for 250 milliseconds
  loop eventStream readModel

handleAndAck :: EventStream MessageId -> ReadModel -> (MessageId, OffsetEvent) -> IO ()
handleAndAck eventStream readModel (eventId, event) = (handle readModel) event *> (ack eventStream) eventId

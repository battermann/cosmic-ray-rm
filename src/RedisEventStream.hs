{-# LANGUAGE OverloadedStrings #-}

module RedisEventStream where

import           Control.Concurrent    (threadDelay)
import           Control.Monad         (void)
import qualified Data.Aeson            as Json
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as BL
import           Data.Maybe            (catMaybes)
import           Database.Redis
import           EventStream
import           Types

newtype Stream = Stream C.ByteString
newtype Group = Group C.ByteString
newtype Consumer = Consumer C.ByteString
newtype MessageId = MessageId C.ByteString deriving (Show)

newRedisEventStreamWithRetries :: Connection -> Stream -> Group -> Consumer -> IO (EventStream MessageId)
newRedisEventStreamWithRetries conn streamName group consumerName = do
  result <- newRedisEventStream conn streamName group consumerName
  case result of
    Left err  -> do
      putStrLn err
      putStrLn "retry in 1000 milliseconds..."
      threadDelay (1000 * 1000) -- wait for 1000 milliseconds
      newRedisEventStreamWithRetries conn streamName group consumerName
    Right v -> pure v

newRedisEventStream :: Connection -> Stream -> Group -> Consumer -> IO (Either String (EventStream MessageId))
newRedisEventStream conn (Stream streamName) (Group group) consumerName = do
  result <- runRedis conn $ xgroupCreate streamName group "0"
  pure $ case result of
    Left (Error "BUSYGROUP Consumer Group name already exists") -> Right eventStream
    Left (Error err) -> Left $ show err
    _ -> Right eventStream
  where
    eventStream = EventStream
                    { query = runRedis conn $ queryStream (Stream streamName) (Group group) consumerName
                    , ack = \(MessageId msgId) -> void $ runRedis conn $ xack streamName group [msgId]
                    }

queryStream :: Stream -> Group -> Consumer -> Redis [(MessageId, OffsetEvent)]
queryStream (Stream streamName) (Group group) (Consumer consumerName) = do
  pendingIds <- pendingMessageIds (Stream streamName) (Group group) (Consumer consumerName)
  pendingMsgs <- readMessages (Stream streamName) pendingIds
  if null pendingMsgs then do
    responseOrError <- xreadGroup group consumerName [(streamName, ">")]
    pure $ case responseOrError of
      Left err -> error $ show err -- crash and restart application
      Right Nothing -> []
      Right (Just response) ->
        catMaybes (toOffsetEvents <$> (response >>= records)) -- handling/logging errors not implemented yet
  else pure pendingMsgs

readMessages :: Stream -> [MessageId] -> Redis [(MessageId, OffsetEvent)]
readMessages (Stream streamName) ids = do
  responseOrError <- traverse (\(MessageId msgId) -> xrange streamName msgId msgId Nothing) ids
  pure $ case sequence responseOrError of
    Right xs -> catMaybes (toOffsetEvents <$> (concat xs)) -- handling/logging errors not implemented yet
    _        -> []

pendingMessageIds :: Stream -> Group -> Consumer -> Redis [MessageId]
pendingMessageIds (Stream streamName) (Group group) (Consumer consumerName) = do
  result <- xpendingDetail streamName group "-" "+" 10 (Just consumerName)
  pure $ case result of
    Right xs -> (MessageId . messageId) <$> xs
    _        -> []

toOffsetEvents :: StreamsRecord -> Maybe (MessageId, OffsetEvent)
toOffsetEvents record = do
  eventPayload <- lookup "data" (keyValues record)
  event <- Json.decode (BL.fromStrict eventPayload) :: Maybe OffsetEvent
  pure (MessageId $ recordId record, event)

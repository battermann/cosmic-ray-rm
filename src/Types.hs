{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson
import           Data.Int
import           Data.Time.Clock (UTCTime)
import           Data.UUID
import           GHC.Generics

data Color = Yellow
    | Red
    deriving (Eq, Generic)

colorToString :: Color -> String
colorToString Red    = "red"
colorToString Yellow = "yellow"

instance Show Color where
    show = colorToString

instance FromJSON Color

newtype ClientId = ClientId UUID deriving (Eq, Show, Generic)

instance FromJSON ClientId

newtype StreamId = StreamId UUID deriving (Eq, Ord, Show, Generic)

instance FromJSON StreamId

newtype Column = Column Int32 deriving (Eq, Ord, Show, Generic)

instance FromJSON Column

data Event = GameCreated StreamId UTCTime ClientId Color
    | GameJoined StreamId UTCTime ClientId
    | YellowPlayed StreamId UTCTime Column
    | RedPlayed StreamId UTCTime Column
    | GameWon StreamId UTCTime ClientId
    | GameTied StreamId UTCTime
    deriving (Show, Eq, Generic)

instance FromJSON Event

newtype Offset = Offset Int64
  deriving (Eq, Show, Generic)

instance FromJSON Offset

data OffsetEvent = OffsetEvent
    { payload :: Event
    , offset  :: Offset
    }
    deriving (Eq, Show, Generic)

instance FromJSON OffsetEvent

data GameState = InProgress
    | YellowWon
    | RedWon
    | Draw
    deriving (Eq, Show, Generic)

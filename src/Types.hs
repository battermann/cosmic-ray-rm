{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Int
import Data.UUID
import GHC.Generics

data Color
  = Yellow
  | Red
  deriving (Eq, Show, Generic)

colorToString :: Color -> String
colorToString Red = "red"
colorToString Yellow = "yellow"

instance FromJSON Color

newtype ClientId = ClientId UUID deriving (Eq, Show, Generic)

instance FromJSON ClientId

newtype StreamId = StreamId UUID deriving (Eq, Ord, Show, Generic)

instance FromJSON StreamId

newtype Column = Column Int32 deriving (Eq, Ord, Show, Generic)

instance FromJSON Column

data Event
  = GameCreated StreamId ClientId Color
  | GameJoined StreamId ClientId
  | YellowPlayed StreamId Column
  | RedPlayed StreamId Column
  | GameWon StreamId ClientId
  | GameTied StreamId
  deriving (Show, Eq, Generic)

instance FromJSON Event

newtype Version = Version Int32
  deriving (Eq, Show, Generic)

instance FromJSON Version

data VersionedEvent = VersionedEvent Version Event
  deriving (Eq, Show, Generic)

instance FromJSON VersionedEvent

data GameState = InProgress
  deriving (Eq, Show, Generic)

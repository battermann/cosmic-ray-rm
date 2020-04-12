module EventStream where

import           Types

data EventStream a = EventStream
    { query :: IO [(a, OffsetEvent)]
    , ack   :: a -> IO ()
    }

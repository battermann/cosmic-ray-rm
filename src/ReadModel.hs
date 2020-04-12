module ReadModel where

import           Types

data ReadModel = ReadModel
    { handle :: OffsetEvent -> IO ()
    }

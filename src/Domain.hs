module Domain 
    ( Task(Task, time, name)
    ) where

import Data.Time

data Task = Task {
     time   :: UTCTime
    ,name   :: String
    } deriving (Show, Eq)

module Task
    ( Event (Start, End, Log)
    , History
    , Task (Task, description, history)
    , defaultTask
    , updateTaskHistory
    ) where

import Data.Time                    (ZonedTime)

data Event
    = Start
    | End
    | Log String
    deriving (Show, Read, Eq)

type History = [(ZonedTime, Event)]

data Task = Task { 
      description :: String
    , history :: History
    } deriving (Show, Read)

defaultTask :: Task
defaultTask = Task { 
      description = "description is missing"
    , history = [] 
    }

updateTaskHistory :: ZonedTime -> Event -> Task -> Task
updateTaskHistory timeStamp event task = task { history = entry : hstry }
    where 
        entry = (timeStamp, event)
        hstry = history task
module Task ( 
    Event (Created, Start, End, Log),
    History,
    Tag,
    Task (Task, tag, description, history),
    updateTaskHistory
    ) where

import Data.Time (LocalTime)

data Event
    = Created
    | Start
    | End
    | Log String
    deriving (Show, Read, Eq)

type History = [(LocalTime, Event)]
type Tag = String

data Task = Task { 
      tag :: Tag
    , description :: String
    , history :: History
    } deriving (Show, Read, Eq)

updateTaskHistory :: LocalTime -> Event -> Task -> Task
updateTaskHistory timeStamp event task = task { history = entry : hstry }
    where 
        entry = (timeStamp, event)
        hstry = history task
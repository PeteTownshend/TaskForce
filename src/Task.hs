module Task
    ( ID
    , Message
    , Event (Start, End)
    , Task (Task, description, history)
    ) where

import Data.Maybe                   (fromMaybe, isJust)
import Data.Time                    (ZonedTime, getZonedTime)

type ID = String
type Message = String

data Event
    = Start
    | End
    deriving (Show, Read, Eq)

data Task = Task { 
      description :: String
    , history :: [(ZonedTime, Event, Message)]
    } deriving (Show, Read)



updateHistory :: Task -> Task
updateHistory task = undefined
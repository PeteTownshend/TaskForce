module AppState
    ( AppState
    , empty
    , addTask
    , activateTask
    , getDescription
    , setDescription
    , getHistory
    , updateHistory
    ) where

import Zipper
import Task
import Data.Time                    (LocalTime)
import Data.Maybe                   (maybe)

type AppState = Zipper Task

empty :: AppState
empty = ([], [])

addTask :: Task -> AppState -> Maybe AppState
addTask = add (\task -> \task' -> tag task == tag task')

activateTask :: Tag -> AppState -> Maybe AppState
activateTask tag' = slideTo (\task -> tag task == tag')

getDescription :: AppState -> String
getDescription = (maybe "no active task" description) . slider

setDescription :: String -> AppState -> AppState
setDescription dscrptn = modify (\t -> t { description = dscrptn })
            
getHistory :: AppState -> History
getHistory = (maybe [] history) . slider

updateHistory :: LocalTime -> Event -> AppState -> AppState
updateHistory timeStamp = modify . (updateTaskHistory timeStamp)
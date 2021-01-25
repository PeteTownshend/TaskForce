module StateM ( 
    StateM, 
    getTasksM,
    newTaskM,
    deleteTaskM,
    getDescriptionM,
    setDescriptionM,
    getHistoryM,
    updateHistoryM
    ) where

import Zipper
import Task
import AppState
import Data.Time                    (LocalTime)
import Control.Monad.State.Strict   (StateT, gets)

type StateM = StateT AppState IO

getTasksM :: StateM [Task]
getTasksM = liftM toList

newTaskM :: LocalTime -> Tag -> StateM AppState
newTaskM timeStamp tag = liftM $ newTask timeStamp tag

deleteTaskM :: Tag -> StateM AppState
deleteTaskM = liftM . deleteTask

getDescriptionM :: StateM String
getDescriptionM = liftM getDescription

setDescriptionM :: String -> StateM AppState
setDescriptionM = liftM . setDescription

getHistoryM :: StateM History
getHistoryM = liftM getHistory

updateHistoryM :: LocalTime -> Event -> StateM AppState
updateHistoryM timeStamp event = liftM $ updateHistory timeStamp event

liftM :: (AppState -> a) -> StateM a
liftM fromAppState = do
    res <- gets fromAppState
    return res
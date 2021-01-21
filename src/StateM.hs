module StateM
    ( StateM
    , getTasksM
    , getDescriptionM
    , setDescriptionM
    , getHistoryM
    , updateHistoryM
    ) where

import Zipper
import Task
import AppState
import Data.Time                    (LocalTime)
import Control.Monad.State.Strict   (StateT, gets)

type StateM = StateT AppState IO

getTasksM :: StateM [Task]
getTasksM = liftM toList

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
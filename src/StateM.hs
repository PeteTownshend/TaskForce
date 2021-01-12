module StateM
    ( StateM
    , getTasksM
    , getDescriptionM
    , setDescriptionM
    , getHistoryM
    , updateHistoryM
    ) where

import Task
import AppState
import qualified Data.Map as Map
import Data.Map                     (Map, toList)
import Data.Time                    (ZonedTime)
import Control.Monad.State.Strict   (StateT, gets, modify)
import Data.Time                    (ZonedTime)

type StateM = StateT AppState IO

getTasksM :: StateM [(String, Task)]
getTasksM = liftM $ toList . tasks

getDescriptionM :: StateM String
getDescriptionM = liftM getDescription

setDescriptionM :: String -> StateM AppState
setDescriptionM = liftM . setDescription

getHistoryM :: StateM [(ZonedTime, Event)]
getHistoryM = liftM getHistory

updateHistoryM :: ZonedTime -> Event -> StateM AppState
updateHistoryM timeStamp event = liftM $ updateHistory timeStamp event

liftM :: (AppState -> a) -> StateM a
liftM fromAppState = do
    res <- gets fromAppState
    return res
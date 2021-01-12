module AppState
    ( AppState (AppState, active, tasks)
    , activeTask
    , getDescription
    , setDescription
    , getHistory
    , updateHistory
    ) where

import Task
import qualified Data.Map as Map
import Data.Map                     (Map, insert, findWithDefault)
import Data.Time                    (ZonedTime)

data AppState = AppState { 
      active :: String
    , tasks :: Map String Task
    } deriving (Show, Read)

activeTask :: AppState -> (String, Task)
activeTask (AppState id tasks) = (id, task)
    where
        task = findWithDefault defaultTask id tasks

getDescription :: AppState -> String
getDescription appState = description task
    where
        (_, task) = activeTask appState

setDescription :: String -> AppState -> AppState
setDescription dscrptn appState = appState { tasks = insert id task' allTasks }
    where
        (id, task) = activeTask appState
        allTasks = tasks appState
        task' = task { description = dscrptn }
            
getHistory :: AppState -> History
getHistory appState = history task
    where
        (_, task) = activeTask appState

updateHistory :: ZonedTime -> Event -> AppState -> AppState
updateHistory timeStamp event appState = appState { tasks = insert id task' allTasks }
    where
        (id, task) = activeTask appState
        allTasks = tasks appState
        task' = updateTaskHistory timeStamp event task 
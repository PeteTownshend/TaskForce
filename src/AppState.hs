module AppState( 
    AppState,
    addTask,
    activateTask,
    newTask,
    deleteTask,
    getDescription,
    setDescription,
    getHistory,
    updateHistory
    ) where

import Zipper ( Zipper, slider, prune, slideTo, modify, add )
import Task
    ( Task(..), Tag, History, Event(Created), updateTaskHistory )
import Data.Time                    (LocalTime)
import Data.Maybe                   (maybe, fromMaybe)

type AppState = Zipper Task 

addTask :: Task -> AppState -> AppState
addTask = add (\task task' -> tag task == tag task')

activateTask :: Tag -> AppState -> Maybe AppState
activateTask tag' = slideTo (\task -> tag task == tag')

newTask :: LocalTime -> Tag -> AppState -> AppState
newTask timeStamp id = addTask task
    where
        task = Task { tag = id, description = "description not given yet", history = [(timeStamp, Created)] }

deleteTask :: Tag -> AppState -> AppState
deleteTask id = prune ((/=) id . tag)

getDescription :: AppState -> String
getDescription = maybe "no active task" description . slider

setDescription :: String -> AppState -> AppState
setDescription dscrptn = modify (\t -> t { description = dscrptn })
            
getHistory :: AppState -> History
getHistory = maybe [] history . slider

updateHistory :: LocalTime -> Event -> AppState -> AppState
updateHistory timeStamp = modify . updateTaskHistory timeStamp
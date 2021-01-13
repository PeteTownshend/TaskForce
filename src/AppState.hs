module AppState
    ( AppState
    , add
    , goFwd
    , goBwd
    , goStart
    , modify
    , activeTask
    , activateTask
    , getDescription
    , setDescription
    , getHistory
    , updateHistory
    ) where

import Task
import Data.Time                    (LocalTime)

type AppState = ([Task], [Task])

add :: Task -> AppState -> AppState
add task (xs, bs) = (task : xs, bs)

goFwd :: AppState -> AppState
goFwd ([], bs) = ([], bs)
goFwd (x : xs, bs) = (xs, x : bs)

goBwd :: AppState -> AppState
goBwd (xs, []) = (xs, [])
goBwd (xs, b : bs) = (b : xs, bs)

goStart :: AppState -> AppState
goStart (xs, []) = (xs, [])
goStart appState = (goStart . goBwd) appState

modify :: (Task -> Task) -> (AppState -> AppState)
modify _ ([], bs) = ([], bs)
modify f (task : tasks, bs) = ((f task) : tasks, bs)

activeTask :: AppState -> Task
activeTask = head . fst

activateTask :: Tag -> AppState -> AppState
activateTask lookupTag appState = activate $ goStart appState
    where
        check state = (tag $ activeTask state) == lookupTag
        activate state = if (check state) then state else (activate $ goFwd state)

getDescription :: AppState -> String
getDescription = description . activeTask

setDescription :: String -> AppState -> AppState
setDescription dscrptn = modify (\t -> t { description = dscrptn })
            
getHistory :: AppState -> History
getHistory = history . activeTask

updateHistory :: LocalTime -> Event -> AppState -> AppState
updateHistory timeStamp event = modify (updateTaskHistory timeStamp event)
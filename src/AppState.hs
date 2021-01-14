module AppState
    ( AppState
    , getTasks
    , add
    , goFwd
    , goBwd
    , goStart
    , modify
    , activeTask
    , exists
    , activateTask
    , getDescription
    , setDescription
    , getHistory
    , updateHistory
    ) where

import Task
import Data.Time                    (LocalTime)
import Data.Maybe                   (maybe)

type AppState = (Task, [Task], [Task])

getTasks :: AppState -> [Task]
getTasks (a, xs, bs) = a : xs ++ bs

add :: Task -> AppState -> Maybe AppState
add task appState = 
    if exists (tag task) appState
    then Nothing
    else Just $ plus appState
    where 
        plus (x, xs, bs) = (task, x : xs, bs)

goFwd :: AppState -> Maybe AppState
goFwd (_, [], _) = Nothing
goFwd (b, a : xs, bs) = Just (a, xs, b : bs)

goBwd :: AppState -> Maybe AppState
goBwd (_, _, []) = Nothing
goBwd (x, xs, a : bs) = Just (a, x : xs, bs)

goStart :: AppState -> AppState
goStart (a, xs, []) = (a, xs, [])
goStart appState = maybe appState goStart (goBwd appState)

modify :: (Task -> Task) -> (AppState -> AppState)
modify f (task, xs, bs) = (f task, xs, bs)

activeTask :: AppState -> Task
activeTask (task, _, _) = task

exists :: Tag -> AppState -> Bool
exists lookupTag appState = check $ goStart appState
    where
        checkActive state = (tag $ activeTask state) == lookupTag
        check state = checkActive state || (maybe False check (goFwd state))

activateTask :: Tag -> AppState -> Maybe AppState
activateTask lookupTag appState = activate $ goStart appState
    where
        checkActive state = (tag $ activeTask state) == lookupTag
        activate state = 
            if checkActive state
            then Just state 
            else (goFwd state) >>= activate

getDescription :: AppState -> String
getDescription = description . activeTask

setDescription :: String -> AppState -> AppState
setDescription dscrptn = modify (\t -> t { description = dscrptn })
            
getHistory :: AppState -> History
getHistory = history . activeTask

updateHistory :: LocalTime -> Event -> AppState -> AppState
updateHistory timeStamp event = modify (updateTaskHistory timeStamp event)
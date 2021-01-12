{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( taskC
    , getTasksC
    ) where


import Task
import AppState
import StateM
import System.Console.StructuredCLI
import Text.Read                    (readMaybe)
import Data.Maybe                   (isJust, fromMaybe)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.State.Strict   (put)
import Data.Time                    (getZonedTime)
import qualified Data.Map as Map
import Data.Map                     (insert, findWithDefault)
import Control.Monad.State.Strict   (gets, put)

taskC :: CommandsT StateM ()
taskC = param "task" "<'task-id'>" parseString setTask >+ do
    getDescriptionC
    setDescriptionC
    getHistoryC
    startC
    endC
    logC
    rootC
    where 
        setTask id = do
            tasks <- gets tasks
            let task = findWithDefault defaultTask id tasks
            timeStamp <- liftIO getZonedTime
            let task' = updateTaskHistory timeStamp Start task
            put AppState { active = id, tasks = insert id task' tasks }
            return NewLevel

rootC :: CommandsT StateM ()
rootC = command "root" "returns to top level" $ do
    timeStamp <- liftIO getZonedTime
    appState <- updateHistoryM timeStamp End
    put appState
    return ToRoot

getTasksC :: CommandsT StateM ()
getTasksC = command "tasks" "returns a list of all tasks" $ do
    tasks <- getTasksM
    mapM_ (liftIO . putStrLn) (map toString tasks)
    return ToRoot
    where
        toString (id, task) = id ++ " -> " ++ (description task)

getDescriptionC :: CommandsT StateM ()
getDescriptionC = command "description" "returns a more detailed description of the task" $ do
    dscrptn <- getDescriptionM
    liftIO . putStrLn $ dscrptn
    return NoAction

setDescriptionC :: CommandsT StateM ()
setDescriptionC = param "set" "<'description'>" parseString set 
    where
        set dscrptn = do
            appState <- setDescriptionM dscrptn
            put appState
            return NoAction

getHistoryC :: CommandsT StateM ()
getHistoryC = command "history" "returns this history of a task" $ do
    hstry <- getHistoryM
    mapM_ (liftIO . putStrLn) (map toString hstry)
    return NoAction
    where
        toString (timeStamp, action) = show timeStamp ++ ": " ++ show action

startC :: CommandsT StateM ()
startC = dropEvent Start
    
endC :: CommandsT StateM ()
endC = dropEvent End

logC :: CommandsT StateM ()
logC = param "log" "<'message'>" parseString setMessage
    where 
        setMessage txt = do
            timeStamp <- liftIO getZonedTime
            appState <- updateHistoryM timeStamp (Log txt)
            put appState
            return NoAction

parseString :: Validator StateM String
parseString = return . readMaybe

dropEvent :: Event -> CommandsT StateM ()
dropEvent event = command (show event) ("tags the " ++ show event ++ " of a task") $ do
    timeStamp <- liftIO getZonedTime
    appState <- updateHistoryM timeStamp event
    put appState
    return NoAction

admin :: CommandsT StateM ()
admin = custom "admin" "administrations of tasks" (parseOneOf options "getDescription to admin") always $
    const (return NoAction)
    where 
        options = ["delete"]
        always  = return True
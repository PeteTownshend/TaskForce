{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( ID
    , Task
    , AppState (AppState, active, tasks)
    , StateM
    , parseString
    , root
    , basic
    , task
    , getDescription
    , setDescription
    , getTasks
    , admin
    ) where

import Task
import AppState
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.State.Strict   (StateT, gets, modify)
import Data.Maybe                   (fromMaybe, isJust)
import System.Console.StructuredCLI
import Text.Read                    (readMaybe)
import qualified Data.Map as Map
import Data.Map                     (Map, insert, lookup, delete, toList)
import Data.Time                    (ZonedTime, getZonedTime, zonedTimeToUTC)

root :: CommandsT StateM ()
root = do
  getTasks
  task
  admin

basic :: CommandsT StateM ()
basic = do
  command "top" "return to the top" top
  command "exit" "go back one level up" exit
  time

time :: CommandsT StateM ()
time = command "time" "return current time" $ do
    time <- liftIO getZonedTime
    liftIO . putStrLn $ show time
    return NoAction

task :: CommandsT StateM ()
task = param "task" "<'task-id'>" parseString setTask >+ do
    getDescription
    setDescription
    getTaskHistory
    where 
        setTask id = do
            tasks <- gets tasks
            let mTask = Map.lookup id tasks
            timeStamp <- liftIO getZonedTime
            if (isJust mTask)
            then do
                let task = fromMaybe Task { description = "description not given yet", history = [(timeStamp, Start, "returning")] }
                modify $ \s -> s { active = id }
            else do
                let task = Task { description = "description not given yet", history = [(timeStamp, Start, "created")] }
                modify $ \s -> s { active = id, tasks = insert id task tasks }
            return NewLevel

{-end :: CommandsT StateM ()
end = command "end" "pauses the task and returns to root" $ do
    (id, mTask, remainingTasks) <- activeTask
    let task = fromMaybe (Task { description = "failed to pause task", history = [] }) mTask
    timeStamp <- liftIO getZonedTime-}

admin :: CommandsT StateM ()
admin = custom "admin" "administrations of tasks" (parseOneOf options "getDescription to admin") always $
    const (return NoAction)
    where 
        options = ["delete"]
        always  = return True
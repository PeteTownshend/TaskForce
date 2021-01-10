{-# LANGUAGE FlexibleContexts #-}

module AppState
    ( AppState (AppState, active, tasks)
    , StateM
    , parseString
    , getDescription
    , setDescription
    , getTasks
    , getTaskHistory
    ) where

import Task
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.State.Strict   (StateT, gets, modify)
import Data.Maybe                   (fromMaybe, isJust)
import System.Console.StructuredCLI
import Text.Read                    (readMaybe)
import qualified Data.Map as Map
import Data.Map                     (Map, insert, lookup, delete, toList)
import Data.Time                    (ZonedTime, getZonedTime, zonedTimeToUTC)

data AppState = AppState { 
      active :: ID
    , tasks :: Map ID Task
    } deriving (Show, Read)

type StateM = StateT AppState IO

parseString :: Validator StateM String
parseString = return . readMaybe

activeTask :: StateM (ID, Maybe Task, Map ID Task)
activeTask = do
    id <- gets active
    tasks <- gets tasks
    let mTask = Map.lookup id tasks
    let remainingTasks = delete id tasks
    return (id, mTask, remainingTasks)

updateHistory :: Task -> Task
updateHistory task = undefined

getDescription :: CommandsT StateM ()
getDescription = command "description" "returns a more detailed description of the task" $ do
    (_, mTask, _) <- activeTask
    liftIO . putStrLn $ show $ fromMaybe "description is missing" (fmap description mTask)
    return NoAction

getTaskHistory :: CommandsT StateM ()
getTaskHistory = command "history" "returns this history of a task" $ do
    (_, mTask, _) <- activeTask
    let hstry = fromMaybe [] (fmap history mTask)
    mapM_ (liftIO . putStrLn) (map toString hstry)
    return NoAction
    where
        toString (timeStamp, action, message) = show timeStamp ++ " " ++ show action ++ ": " ++ message

setDescription :: CommandsT StateM ()
setDescription = param "set" "<'description'>" parseString setDesc
    where
        setDesc d = do
            (id, mTask, remainingTasks) <- activeTask
            let task = fromMaybe (Task { description = "failed to set description '" ++ d ++ "'", history = [] }) (fmap (\t -> t { description = d }) mTask)
            modify $ \s -> s { tasks = insert id task remainingTasks }
            return NoAction

getTasks :: CommandsT StateM ()
getTasks = command "tasks" "returns a list of all tasks" $ do
    tasks <- gets tasks
    let list = toList tasks
    mapM_ (liftIO . putStrLn) (map toString list)
    return ToRoot
    where
        toString (id, task) = id ++ " -> " ++ (description task)
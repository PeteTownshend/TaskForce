{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module Main where

import Control.Monad.IO.Class       (liftIO)
import Control.Monad.State.Strict   (StateT, evalStateT, gets, modify)
import Data.Default                 (def)
import Data.Maybe                   (fromMaybe)
import System.Console.StructuredCLI
import Text.Read                    (readMaybe)
import qualified Data.Map as Map
import Data.Map                     (Map, insert, lookup)

type ID = String

data Task = Task { 
    description :: String 
    } deriving (Show, Read, Eq)

data AppState = AppState { 
     active :: ID
    ,tasks :: Map ID Task
    } deriving (Show, Read)

type StateM = StateT AppState IO

parseString :: Validator StateM String
parseString = return . readMaybe

root :: CommandsT StateM ()
root = do
  basic
  task
  admin

basic :: CommandsT StateM ()
basic = do
  command "top" "return to the top" top
  command "exit" "go back one level up" exit

task :: CommandsT StateM ()
task = param "task" "<'task-id'>" parseString setTask >+ do
    basic
    getDescription
    setDescription
    where 
        setTask id = do
            tasks <- gets tasks
            let task = Task { description = "description not given yet" }
            modify $ \s -> s { active = id, tasks = insert id task tasks }
            return NewLevel

getDescription :: CommandsT StateM ()
getDescription = command' "description" "returns a more detailed description of the task" (return True) $ do
    id <- gets active
    tasks <- gets tasks
    let mTask = Map.lookup id tasks
    liftIO . putStrLn $ show $ fromMaybe "description is missing" (fmap description mTask)
    return NoAction

setDescription :: CommandsT StateM ()
setDescription = param "set" "<'description'>" parseString setDesc
    where
        setDesc d = do
            id <- gets active
            tasks <- gets tasks
            let mTask = Map.lookup id tasks
            let task = fromMaybe (Task $ "failed to set description '" ++ d ++ "'") (fmap (\t -> t { description = d }) mTask)
            modify $ \s -> s { active = id, tasks = insert id task tasks }
            return NoAction

admin :: CommandsT StateM ()
admin = custom "admin" "administrations of tasks" (parseOneOf options "getDescription to admin") always $
    const (return NoAction)
    where 
        options = ["delete"]
        always  = return True

main :: IO ()
main = do
    let state0 = AppState "" Map.empty
    evalStateT run state0
    where 
        run = do
            result <- runCLI "" settings root
            either (error.show) return result
        settings = def { getBanner = "CLI for task management", getHistory = Just ".taskForceCLI.history" }

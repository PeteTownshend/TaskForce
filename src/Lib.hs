{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( taskC
    , getTasksC
    , appStateFileName
    , shutdownC
    , parseString
    ) where

import Task
import AppState
import StateM
import System.Console.StructuredCLI
import Text.Read                    (readMaybe)
import Data.Maybe                   (fromMaybe)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.State.Strict   (put)
import Control.Monad.State.Strict   (get, put)
import System.IO                    (openTempFile, hPutStr, hClose)
import System.Directory             (removeFile, renameFile)
import Control.Exception            (bracketOnError)
import Data.Char                    (isSpace)

appStateFileName :: String
appStateFileName = ".taskForceCLI.state"

taskC :: CommandsT StateM ()
taskC = param "task" "<'task-id'>" parseString setTask >+ do
    getDescriptionC
    setDescriptionC
    getHistoryC
    logC
    rootC
    where 
        setTask id = do
            appState <- get
            timeStamp <- liftIO getLocalTime
            let defaultTask = Task { tag = id, description = "description not given yet", history = [] }
                defaultAppState = fromMaybe ([], [defaultTask]) (addTask defaultTask appState)
                appState' = fromMaybe defaultAppState (activateTask id appState)
                appState'' = updateHistory timeStamp Start appState'
            put appState''                
            return NewLevel

shutdownC :: CommandsT StateM ()
shutdownC = command "shutdown" "exits application and stores state" $ do
    timeStamp <- liftIO getLocalTime
    appState <- get
    liftIO $ store appState
    return NoAction
    where 
        store state = do 
            bracketOnError (openTempFile "." "temp") (
                \(tempName, tempHandle) -> do
                    hClose tempHandle
                    removeFile tempName) (
                \(tempName, tempHandle) -> do
                    hPutStr tempHandle $ show state        
                    hClose tempHandle
                    removeFile appStateFileName
                    renameFile tempName appStateFileName)

rootC :: CommandsT StateM ()
rootC = command "exit" "returns to top level" $ do
    timeStamp <- liftIO getLocalTime
    appState <- updateHistoryM timeStamp End
    put appState
    return ToRoot

getTasksC :: CommandsT StateM ()
getTasksC = command "tasks" "returns a list of all tasks" $ do
    tasks <- getTasksM
    mapM_ (liftIO . putStrLn) (map toString tasks)
    return NoAction
    where
        toString task = (tag task) ++ " -> " ++ (description task)

getDescriptionC :: CommandsT StateM ()
getDescriptionC = command "description" "returns a more detailed description of the task" $ do
    dscrptn <- getDescriptionM
    liftIO . putStrLn $ dscrptn
    return NoAction

setDescriptionC :: CommandsT StateM ()
setDescriptionC = paramSentence "set" "<'description'>" parseString set 
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

logC :: CommandsT StateM ()
logC = param "log" "<'message'>" parseString setMessage
    where 
        setMessage txt = do
            timeStamp <- liftIO getLocalTime
            appState <- updateHistoryM timeStamp (Log txt)
            put appState
            return NoAction

parseString :: Validator StateM String
parseString = return . readMaybe

paramSentence :: (Monad m) => String -- ^ Command keyword
                   -> String         -- ^ Help text for this command (including argument description)
                   -> Validator m a  -- ^ Monadic validator (in the "user" monad)
                   -> Handler m a    -- ^ Handling action. Takes the validator output as argument
                   -> CommandsT m ()
paramSentence label hint validator handler =
    paramSentence' label hint validator (return True) handler

paramSentence' :: (Monad m) => String -- ^ Command keyword
                    -> String         -- ^ Help text for this command (including argument description)
                    -> Validator m a  -- ^ Monadic validator (in the "user" monad)
                    -> m Bool         -- ^ Enable action in the "user" monad
                    -> Handler m a    -- ^ Handling action. Takes the validator output as argument
                    -> CommandsT m ()
paramSentence' label hint validator enable handler = do
  custom label hint parser enable handler
         where parser = sentenceParser hint validator

sentenceParser :: Monad m => String -> (String -> m (Maybe a)) -> Node m -> String -> m (ParseResult a)
sentenceParser hint validator = parseParam -.- labelParser
    where parseParam  = (=<<) parseParam'
          parseParam' (Done _ matched rest) =
              case rest of
                "?" ->
                  return $ Fail hint rest
                "" ->
                  return $ Partial [("", hint)] ""
                word -> do
                  v <- validator word
                  return $ maybe (badArg rest) (\x -> Done x (matched ++ ' ':word) "") v
          parseParam' (Fail x y) =
              return $ Fail x y
          parseParam' (Partial x y) =
              return $ Partial x y
          parseParam' NoMatch = return NoMatch
          badArg = Fail hint

infixr 9 -.-
(-.-) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(-.-) = (.).(.)

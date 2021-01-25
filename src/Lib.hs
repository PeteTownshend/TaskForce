{-# LANGUAGE FlexibleContexts #-}

module Lib ( 
    appStateFileName,
    taskCommand,
    tasksCommand,
    deleteCommand,
    reportCommand,
    quitCommand,
    runCommand,
    getLocalTime
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
import Data.Time
import Report

appStateFileName :: String
appStateFileName = ".taskForceCLI.state"

getLocalTime :: IO LocalTime
getLocalTime = do
    utcTime <- getCurrentTime
    timeZone <- getTimeZone utcTime
    return $ utcToLocalTime timeZone utcTime

taskCommand :: CommandsT StateM ()
taskCommand = param "task" "<'tag'>" parseString setTask
    where 
        setTask id = do
            timeStamp <- liftIO getLocalTime
            appState <- newTaskM timeStamp id
            put appState                
            return NoAction

tasksCommand :: CommandsT StateM ()
tasksCommand = command "tasks" "prints a list of all tasks" $ do
    tasks <- getTasksM
    mapM_ (liftIO . putStrLn) (map toString tasks)
    return NoAction
    where
        toString task = (tag task) ++ " -> " ++ (description task)

deleteCommand :: CommandsT StateM ()
deleteCommand = param "delete" "<'tag'>" parseString delete
    where
        delete id = do
            appState <- deleteTaskM id
            put appState
            return NoAction

reportCommand :: CommandsT StateM ()
reportCommand = command "report" "prints a report" $ do
    appState <- get
    mapM_ (liftIO . putStrLn) (map show $ reportStrings $ report appState)
    return NoAction

quitCommand :: CommandsT StateM () -- TODO: exit is missing
quitCommand = command "quit" "stores state and exits application" $ do
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

runCommand :: CommandsT StateM ()
runCommand = param "run" "<'tag'>" parseString setTask >+ do
    setDescriptionCommand
    getDescriptionCommand
    logCommand
    getHistoryCommand
    exitCommand
    where 
        setTask id = do
            timeStamp <- liftIO getLocalTime
            appState <- newTaskM timeStamp id
            let appState' = fromMaybe appState' (activateTask id appState)
                appState'' = updateHistory timeStamp Start appState'
            put appState''
            return NewLevel

setDescriptionCommand :: CommandsT StateM ()
setDescriptionCommand = paramSentence "set" "<'description'>" parseString set 
    where
        set dscrptn = do
            appState <- setDescriptionM dscrptn
            put appState
            return NoAction

getDescriptionCommand :: CommandsT StateM ()
getDescriptionCommand = command "description" "returns a more detailed description of the task" $ do
    dscrptn <- getDescriptionM
    liftIO . putStrLn $ dscrptn
    return NoAction

logCommand :: CommandsT StateM ()
logCommand = paramSentence "log" "<'message'>" parseString setMessage
    where 
        setMessage txt = do
            timeStamp <- liftIO getLocalTime
            appState <- updateHistoryM timeStamp (Log txt)
            put appState
            return NoAction

getHistoryCommand :: CommandsT StateM ()
getHistoryCommand = command "history" "returns this history of a task" $ do
    hstry <- getHistoryM
    mapM_ (liftIO . putStrLn) (map toString hstry)
    return NoAction
    where
        toString (timeStamp, action) = showLocalTime timeStamp ++ " > " ++ show action

exitCommand :: CommandsT StateM ()
exitCommand = command "exit" "returns to top level" $ do
    timeStamp <- liftIO getLocalTime
    appState <- updateHistoryM timeStamp End
    put appState
    return ToRoot

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

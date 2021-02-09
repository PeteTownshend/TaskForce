{-# LANGUAGE FlexibleContexts #-}

module StateM (
    rootCommands,
    getLocalTime,
    appStateFileName) where

import Zipper                       (Zipper, toList)
import Task (
    Tag,
    Task(tag, description), 
    Event(End, Start, Log))
import AppState (
    AppState,
    activateTask,
    newTask,
    deleteTask,
    getDescription,
    setDescription,
    getHistory,
    updateHistory)
import System.Console.StructuredCLI (
    (>+),
    command,
    custom,
    labelParser,
    param,
    parseOneOf,
    Action(ToRoot, NewLevel, NoAction),
    CommandsT(..),
    Handler,
    Node,
    ParseResult(Fail, Done, Partial, NoMatch),
    Validator)
import Text.Read                    (readMaybe)
import Data.Maybe                   (fromMaybe)
import Control.Monad.State.Class    (MonadState)
import Control.Monad.IO.Class       (liftIO, MonadIO)
import Control.Monad.State.Strict   (join, get, put, gets, StateT, lift)
import System.IO                    (openTempFile, hPutStr, hClose)
import System.Directory             (removeFile, renameFile)
import Control.Exception            (bracketOnError)
import Data.Char                    (isSpace)
import Data.Time                    (getCurrentTime, utcToLocalTime, getTimeZone, LocalTime)
import Report                       (showLocalTime, report, reportStrings)
import Lib                          (paramSentence)

appStateFileName :: String
appStateFileName = ".taskForceCLI.state"

type StateM = StateT AppState IO

rootCommands :: CommandsT StateM ()
rootCommands = do
    newTaskCommand
    runExistingTaskCommand
    listTasksCommand
    deleteTaskCommand
    reportCommand
    quitCommand

newTaskCommand :: CommandsT StateM ()
newTaskCommand = param "new" "<'tag'>" parseString setTask >+ do runningTaskCommands

runExistingTaskCommand :: CommandsT StateM ()
runExistingTaskCommand = do
    tasks <- lift $ gets toList
    let always = return True
        parser = parseOneOf (fmap tag tasks) "task to run"
    custom "run" "runs an existing task" parser always setTask >+ do runningTaskCommands

listTasksCommand :: CommandsT StateM ()
listTasksCommand = command "tasks" "prints a list of all tasks" $ do
    tasks <- gets toList
    mapM_ (liftIO . putStrLn . tag) tasks
    return NoAction

deleteTaskCommand :: CommandsT StateM ()
deleteTaskCommand = param "delete" "<'tag'>" parseString delete
    where 
        delete id = do
            appState <- gets $ deleteTask id
            put appState
            return NoAction

reportCommand :: CommandsT StateM ()
reportCommand = command "report" "prints a report" $ do
    appState <- get
    mapM_ (liftIO . print) (reportStrings $ report appState)
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

runningTaskCommands :: CommandsT StateM ()
runningTaskCommands = do
    setDescriptionCommand
    getDescriptionCommand
    logCommand
    getHistoryCommand
    exitCommand

setDescriptionCommand :: CommandsT StateM ()
setDescriptionCommand = paramSentence "set" "<'description'>" parseString setD
    where
        setD dscrptn = do
            appState <- gets $ setDescription dscrptn
            put appState
            return NoAction

getDescriptionCommand :: CommandsT StateM ()
getDescriptionCommand = command "description" "returns a more detailed description of the task" $ do
    dscrptn <- gets getDescription
    liftIO . putStrLn $ dscrptn
    return NoAction

logCommand :: CommandsT StateM ()
logCommand = paramSentence "log" "<'message'>" parseString setMessage
    where 
        setMessage txt = do
            timeStamp <- liftIO getLocalTime
            appState  <- gets $ updateHistory timeStamp (Log txt)
            put appState
            return NoAction

getHistoryCommand :: CommandsT StateM ()
getHistoryCommand = command "history" "returns this history of a task" $ do
    hstry <- gets getHistory
    mapM_ (liftIO . putStrLn . toString) hstry
    return NoAction
    where
        toString (timeStamp, action) = showLocalTime timeStamp ++ " > " ++ show action

exitCommand :: CommandsT StateM ()
exitCommand = command "exit" "returns to top level" $ do
    timeStamp <- liftIO getLocalTime
    appState  <- gets $ updateHistory timeStamp End
    put appState
    return ToRoot

getLocalTime :: IO LocalTime
getLocalTime = do
    utcTime  <- getCurrentTime
    timeZone <- getTimeZone utcTime
    return $ utcToLocalTime timeZone utcTime

parseString :: Validator StateM String
parseString = return . readMaybe

setTask :: (MonadIO m, MonadState AppState m) => Tag -> m Action
setTask id = do
    timeStamp <- liftIO getLocalTime
    appState  <- gets $ newTask timeStamp id
    let appState'  = fromMaybe appState (activateTask id appState)
        appState'' = updateHistory timeStamp Start appState'
    put appState''
    return NewLevel
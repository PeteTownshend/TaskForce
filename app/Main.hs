module Main where

import Task ()
import AppState ()
import StateM                       (appStateFileName, rootCommands, appStateFileName)    
import Control.Monad.State.Strict   (evalStateT)
import Data.Default                 (def)
import qualified System.Console.StructuredCLI as CLI

logo :: String
logo = unlines [
    "   __             __   ______                    ",
    "  / /_____ ______/ /__/ ____/___  _____________  ",
    " / __/ __ `/ ___/ //_/ /_  / __ \\/ ___/ ___/ _ \\ ",
    "/ /_/ /_/ (__  ) ,< / __/ / /_/ / /  / /__/  __/ ",
    "\\__/\\__,_/____/_/|_/_/    \\____/_/   \\___/\\___/  "
    ]

main :: IO ()
main = do
    appStateContent <- readFile appStateFileName
    let appState = read appStateContent
    evalStateT run appState
    where 
        run = do
            result <- CLI.runCLI "" settings $ do rootCommands
            either (error.show) return result
        settings = def { 
            CLI.getBanner = logo ++ "\nCLI for task management",
            CLI.getHistory = Just ".taskForceCLI.history"
        }

{-
h
5 nach h
10 nach h
viertel nach h
20 nach h
5 vor halb h+
halb h+
5 nach halb h+
20 vor  h+
viertel vor h+
10 vor h+
5 vor h+
-}
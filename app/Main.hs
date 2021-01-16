module Main where

import Task
import AppState
import Lib
import Control.Monad.State.Strict   (evalStateT)
import Data.Default                 (def)
import qualified System.Console.StructuredCLI as CLI

main :: IO ()
main = do
    appStateContent <- readFile appStateFileName
    let appState = read appStateContent
    evalStateT run appState
    where 
        run = do
            result <- CLI.runCLI "" settings $ do
                getTasksC
                taskC
                shutdownC
            either (error.show) return result
        settings = def { 
              CLI.getBanner = "CLI for task management"
            , CLI.getHistory = Just ".taskForceCLI.history"
        }
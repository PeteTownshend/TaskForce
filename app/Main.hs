module Main where

import Task
import AppState
import Lib
import Control.Monad.State.Strict   (evalStateT)
import Data.Default                 (def)
import qualified System.Console.StructuredCLI as CLI

logo :: String
logo = unlines [
      "   __             __   ______                    "
    , "  / /_____ ______/ /__/ ____/___  _____________  "
    , " / __/ __ `/ ___/ //_/ /_  / __ \\/ ___/ ___/ _ \\ "
    , "/ /_/ /_/ (__  ) ,< / __/ / /_/ / /  / /__/  __/ "
    , "\\__/\\__,_/____/_/|_/_/    \\____/_/   \\___/\\___/  "
    ]

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
              CLI.getBanner = logo ++ "\nCLI for task management"
            , CLI.getHistory = Just ".taskForceCLI.history"
        }
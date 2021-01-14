module Main where

import Task
import AppState
import Lib
import Control.Monad.State.Strict   (evalStateT)
import Data.Default                 (def)
import qualified System.Console.StructuredCLI as CLI
import qualified Data.Map as Map

main :: IO ()
main = do
    let task0 = Task { tag = "root", description = "state can't be empty", history = [] }
        state0 = (task0, [], [])
    evalStateT run state0
    where 
        run = do
            result <- CLI.runCLI "" settings $ do
                getTasksC
                taskC
            either (error.show) return result
        settings = def { 
            CLI.getBanner = "CLI for task management", 
            CLI.getHistory = Just ".taskForceCLI.history" 
            }
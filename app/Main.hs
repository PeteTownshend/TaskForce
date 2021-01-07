module Main where

import Lib
import Control.Monad.State.Strict   (evalStateT)
import Data.Default                 (def)
import System.Console.StructuredCLI
import qualified Data.Map as Map

main :: IO ()
main = do
    let state0 = AppState { active = "", tasks = Map.empty }
    evalStateT run state0
    where 
        run = do
            result <- runCLI "" settings root
            either (error.show) return result
        settings = def { getBanner = "CLI for task management", getHistory = Just ".taskForceCLI.history" }

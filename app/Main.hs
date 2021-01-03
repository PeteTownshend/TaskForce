module Main where

import Domain
import Data.Time
import System.Environment

helpMessage :: String
helpMessage = "Usage of task:\n\
\  -new TASKNAME\n\
\    to create a new task named TASKNAME.\n\
\  -help\n\
\    returns this list of arguments."

help :: IO ()
help = putStrLn helpMessage

parseNewTask :: [String] -> IO Task
parseNewTask ("-new" : name : _) = do
    time <- getCurrentTime
    pure $ Task { time = time, name = name }
parseNewTask (_ : xs) = parseNewTask xs
parseNewTask [] = fail "task must be specified in args. see help."

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-help"] -> help
    _ -> main' args

main' :: [String] -> IO ()
main' args = do 
  task <- parseNewTask args
  print (task)

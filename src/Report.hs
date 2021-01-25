module Report (
    report,
    showLocalTime,
    reportStrings,
    accumulate,
    consumption,
    getMinutes
    ) where

import Zipper
import Task
import AppState
import Data.Time
import Data.Fixed                   (div', divMod')
import Data.Either                  (fromRight)

type Report = [(Tag, String, LocalTime, (Int, Int))]

showLocalTime :: LocalTime -> String
showLocalTime =  formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

getMinutes :: LocalTime -> LocalTime -> Int
getMinutes t1 t2 = div' seconds 60
    where
        seconds = nominalDiffTimeToSeconds $ diffLocalTime t2 t1

type Period = Either LocalTime Int

accumulate :: [Period] -> (LocalTime, Event) -> [Period]
accumulate periods (end, End) = Left end : periods
accumulate (Left end : periods) (start, Start) = Right (getMinutes start end) : periods
accumulate periods _ = periods

minutes :: Int -> Period -> Int
minutes acc (Left _) = acc
minutes acc (Right m) = acc + m

consumption :: History -> (Int, Int)
consumption history = divMod' minutes 60
    where 
        accumulated = foldl accumulate [] history
        minutes = foldl (\acc period -> acc + fromRight 0 period) 0 accumulated

created :: History -> LocalTime
created ((t, Created) : _) = t
created (_ : history) = created history

report :: AppState -> Report
report = map toLine . toList
    where
        toLine task = (
            tag task, 
            description task, 
            created $ history task,
            consumption $ history task
            )

reportStrings :: Report -> [String]
reportStrings = map toString
    where
        toString (tag, description, created, (hs, _)) 
            = showLocalTime created ++ " " ++ show hs ++ " hours, " ++ tag ++ ", " ++ description
import Test.Hspec

import Task
import AppState
import StateM
import qualified Data.Map as Map
import Data.Map                     (Map, findWithDefault, empty)
import Data.Time                    (getZonedTime)

main :: IO ()
main = hspec $ do

    describe "show & read" $ do

        it "should return identity" $ do
            timeStamp <- getZonedTime
            let task = Task { description = "details", history = [(timeStamp, Start)] }
                id = "one"
                map = Map.fromList [(id, task)]
                appState = AppState {
                  active = id
                , tasks = map
                }
                newAppState = read (show appState) :: AppState
                newTasks = tasks newAppState
                newTask = findWithDefault defaultTask id newTasks
                newActive = active newAppState
                oldActive = active appState
            
            --newActive `shouldBe` oldActive
            --(description newTask) `shouldBe` (description task)
            --(head $ history newTask) `shouldBe` (history task)
            2 * 3 `shouldBe` 6
module AppStateSpec where

import Test.Hspec
import Task
import AppState

spec :: Spec
spec = do
    
    let task0 = Task { tag = "0", description = "zero",  history = [] }
        task1 = Task { tag = "1", description = "one",   history = [] }
        task2 = Task { tag = "2", description = "two",   history = [] }
        task3 = Task { tag = "3", description = "three", history = [] }
        task4 = Task { tag = "4", description = "four",  history = [] }

        testAppState :: AppState -> ([Tag], [Tag])
        testAppState (xs, bs) = (map tag xs, map tag bs)

    describe "add" $ do

        let appState = ([task1, task2, task3, task4], [])
            appStateN i = add task0 $ iterate goFwd appState !! i

        it "should add at active position" $ do

            appStateN 0 `shouldBe` ([task0, task1, task2, task3, task4], [])
            appStateN 1 `shouldBe` ([task0, task2, task3, task4], [task1])
            appStateN 2 `shouldBe` ([task0, task3, task4], [task2, task1])
            appStateN 3 `shouldBe` ([task0, task4], [task3, task2, task1])
            appStateN 4 `shouldBe` ([task0], [task4, task3, task2, task1])

    describe "goFwd" $ do

        let appState = ([task0, task1, task2, task3, task4], []) 
            appStateN i = iterate goFwd appState !! i

        it "should go right" $ do
            
            appStateN 1 `shouldBe` ([task1, task2, task3, task4], [task0])
            appStateN 2 `shouldBe` ([task2, task3, task4], [task1, task0])
            appStateN 3 `shouldBe` ([task3, task4], [task2, task1, task0])
            appStateN 4 `shouldBe` ([task4], [task3, task2, task1, task0])
            appStateN 5 `shouldBe` ([], [task4, task3, task2, task1, task0])
            appStateN 6 `shouldBe` ([], [task4, task3, task2, task1, task0])
               
    describe "goBwd" $ do

        let appState = ([], [task4, task3, task2, task1, task0])
            appStateN i = iterate goBwd appState !! i

        it "should go left" $ do
            
            appStateN 1 `shouldBe` ([task4], [task3, task2, task1, task0])
            appStateN 2 `shouldBe` ([task3, task4], [task2, task1, task0])
            appStateN 3 `shouldBe` ([task2, task3, task4], [task1, task0])
            appStateN 4 `shouldBe` ([task1, task2, task3, task4], [task0])
            appStateN 5 `shouldBe` ([task0, task1, task2, task3, task4], [])
            appStateN 6 `shouldBe` ([task0, task1, task2, task3, task4], [])
      
    describe "goStart" $ do

        let appState = ([], [task4, task3, task2, task1, task0])
            appStateN i = goStart $ iterate goBwd appState !! i
            res = ([task0, task1, task2, task3, task4], [])

        it "should go to the far left" $ do
            
            appStateN 0 `shouldBe` res
            appStateN 1 `shouldBe` res
            appStateN 2 `shouldBe` res
            appStateN 3 `shouldBe` res
            appStateN 4 `shouldBe` res
            appStateN 5 `shouldBe` res
            appStateN 6 `shouldBe` res

        it "should stay on the far left" $ do

            goStart ([task0], []) `shouldBe` ([task0], [])

    describe "modify" $ do

        let appState = ([task2, task3, task4], [task1, task0]) 
            task5 = Task { tag = "5", description = "two",  history = [] }

        it "should change the active task" $ do

            modify (\t -> t { tag = "5" }) appState `shouldBe` ([task5, task3, task4], [task1, task0]) 

    describe "activeTask" $ do

        let appState = ([task2, task3, task4], [task1, task0])

        it "should return the activeTask" $ do

            activeTask appState `shouldBe` task2

    describe "activateTask" $ do

        it "should return AppState correctly activated" $ do

            let ref = ([task0, task1, task2, task3, task4], [])

            activateTask "0" ([task0, task1, task2, task3, task4], []) `shouldBe` ref
            activateTask "0" ([task1, task2, task3, task4], [task0])   `shouldBe` ref
            activateTask "0" ([task2, task3, task4], [task1, task0])   `shouldBe` ref
            activateTask "0" ([task3, task4], [task2, task1, task0])   `shouldBe` ref
            activateTask "0" ([task4], [task3, task2, task1, task0])   `shouldBe` ref
            activateTask "0" ([], [task4, task3, task2, task1, task0]) `shouldBe` ref

        it "should return AppState correctly activated even from mid term" $ do

            let ref = ([task2, task3, task4], [task1, task0])

            activateTask "2" ([task0, task1, task2, task3, task4], []) `shouldBe` ref
            activateTask "2" ([task1, task2, task3, task4], [task0])   `shouldBe` ref
            activateTask "2" ([task2, task3, task4], [task1, task0])   `shouldBe` ref
            activateTask "2" ([task3, task4], [task2, task1, task0])   `shouldBe` ref
            activateTask "2" ([task4], [task3, task2, task1, task0])   `shouldBe` ref
            activateTask "2" ([], [task4, task3, task2, task1, task0]) `shouldBe` ref

        it "should return AppState correctly activated even from end" $ do

            let ref = ([task4], [task3, task2, task1, task0])

            activateTask "4" ([task0, task1, task2, task3, task4], []) `shouldBe` ref
            activateTask "4" ([task1, task2, task3, task4], [task0])   `shouldBe` ref
            activateTask "4" ([task2, task3, task4], [task1, task0])   `shouldBe` ref
            activateTask "4" ([task3, task4], [task2, task1, task0])   `shouldBe` ref
            activateTask "4" ([task4], [task3, task2, task1, task0])   `shouldBe` ref
            activateTask "4" ([], [task4, task3, task2, task1, task0]) `shouldBe` ref

        it "should stick to activated already" $ do

            activateTask "0" ([task0], []) `shouldBe` ([task0], [])
            activateTask "0" ([task0, task1], []) `shouldBe` ([task0, task1], [])

    describe "read . show" $ do

        it "should be an identity" $ do
            
            timeStamp <- getLocalTime
            let task t = Task { 
                      tag = t
                    , description = "details" ++ t
                    , history = [(timeStamp, Start)] 
                }
                appState = ([task "two", task "three", task "four"], [task "one", task "zero"])
                newAppState = read (show appState) :: AppState
    
            newAppState `shouldBe` appState
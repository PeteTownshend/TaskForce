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

    describe "add" $ do

        it "should add at active position" $ do

            add task0 (task1, [task2, task3, task4], []) `shouldBe` Just (task0, [task1, task2, task3, task4], [])
            add task0 (task2, [task3, task4], [task1])   `shouldBe` Just (task0, [task2, task3, task4], [task1])
            add task0 (task3, [task4], [task2, task1])   `shouldBe` Just (task0, [task3, task4], [task2, task1])
            add task0 (task4, [], [task3, task2, task1]) `shouldBe` Just (task0, [task4], [task3, task2, task1])
            add task2 (task1, [task2, task3, task4], []) `shouldBe` Nothing
            add task2 (task2, [task3, task4], [task1])   `shouldBe` Nothing
            add task2 (task3, [task4], [task2, task1])   `shouldBe` Nothing
            add task2 (task4, [], [task3, task2, task1]) `shouldBe` Nothing

    describe "goFwd" $ do

        it "should go right" $ do
            
            goFwd (task0, [task1, task2, task3, task4], []) `shouldBe` Just (task1, [task2, task3, task4], [task0])
            goFwd (task1, [task2, task3, task4], [task0])   `shouldBe` Just (task2, [task3, task4], [task1, task0])
            goFwd (task2, [task3, task4], [task1, task0])   `shouldBe` Just (task3, [task4], [task2, task1, task0])
            goFwd (task3, [task4], [task2, task1, task0])   `shouldBe` Just (task4, [], [task3, task2, task1, task0])
            goFwd (task4, [], [task3, task2, task1, task0]) `shouldBe` Nothing
               
    describe "goBwd" $ do

        it "should go left" $ do
            
            goBwd (task4, [], [task3, task2, task1, task0]) `shouldBe` Just (task3, [task4], [task2, task1, task0])
            goBwd (task3, [task4], [task2, task1, task0])   `shouldBe` Just (task2, [task3, task4], [task1, task0])
            goBwd (task2, [task3, task4], [task1, task0])   `shouldBe` Just (task1, [task2, task3, task4], [task0])
            goBwd (task1, [task2, task3, task4], [task0])   `shouldBe` Just (task0, [task1, task2, task3, task4], [])
            goBwd (task0, [task1, task2, task3, task4], []) `shouldBe` Nothing
      
    describe "goStart" $ do

        it "should go to the far left" $ do

            let res = (task0, [task1, task2, task3, task4], [])
            
            goStart (task4, [], [task3, task2, task1, task0]) `shouldBe` res
            goStart (task3, [task4], [task2, task1, task0])   `shouldBe` res
            goStart (task2, [task3, task4], [task1, task0])   `shouldBe` res
            goStart (task1, [task2, task3, task4], [task0])   `shouldBe` res
            goStart (task0, [task1, task2, task3, task4], []) `shouldBe` res

    describe "modify" $ do

        it "should change the active task" $ do

            let task5 = Task { tag = "5", description = "two", history = [] }
                ref = (task5, [task3, task4], [task1, task0])
                inp = (task2, [task3, task4], [task1, task0])

            modify (\t -> t { tag = "5" }) inp `shouldBe` ref

    describe "activeTask" $ do

        it "should return the activeTask" $ do

            activeTask (task2, [task3, task4], [task1, task0])    `shouldBe` task2
            activeTask (task4, [], [task3, task2, task1, task0])  `shouldBe` task4

    describe "exists" $ do

        it "should return True if a task exists already" $ do

            exists "0" (task0, [task1, task2, task3, task4], []) `shouldBe` True
            exists "0" (task1, [task2, task3, task4], [task0])   `shouldBe` True
            exists "0" (task2, [task3, task4], [task1, task0])   `shouldBe` True
            exists "0" (task3, [task4], [task2, task1, task0])   `shouldBe` True
            exists "0" (task4, [], [task3, task2, task1, task0]) `shouldBe` True
            exists "2" (task0, [task1, task2, task3, task4], []) `shouldBe` True
            exists "2" (task1, [task2, task3, task4], [task0])   `shouldBe` True
            exists "2" (task2, [task3, task4], [task1, task0])   `shouldBe` True
            exists "2" (task3, [task4], [task2, task1, task0])   `shouldBe` True
            exists "2" (task4, [], [task3, task2, task1, task0]) `shouldBe` True
            exists "4" (task0, [task1, task2, task3, task4], []) `shouldBe` True
            exists "4" (task1, [task2, task3, task4], [task0])   `shouldBe` True
            exists "4" (task2, [task3, task4], [task1, task0])   `shouldBe` True
            exists "4" (task3, [task4], [task2, task1, task0])   `shouldBe` True
            exists "4" (task4, [], [task3, task2, task1, task0]) `shouldBe` True

        it "should return False if a task doesn't exist" $ do

            exists "5" (task0, [task1, task2, task3, task4], []) `shouldBe` False
            exists "5" (task1, [task2, task3, task4], [task0])   `shouldBe` False
            exists "5" (task2, [task3, task4], [task1, task0])   `shouldBe` False
            exists "5" (task3, [task4], [task2, task1, task0])   `shouldBe` False
            exists "5" (task4, [], [task3, task2, task1, task0]) `shouldBe` False

    describe "activateTask" $ do

        it "should return AppState correctly activated" $ do

            let ref = Just (task0, [task1, task2, task3, task4], [])

            activateTask "0" (task0, [task1, task2, task3, task4], []) `shouldBe` ref
            activateTask "0" (task1, [task2, task3, task4], [task0])   `shouldBe` ref
            activateTask "0" (task2, [task3, task4], [task1, task0])   `shouldBe` ref
            activateTask "0" (task3, [task4], [task2, task1, task0])   `shouldBe` ref
            activateTask "0" (task4, [], [task3, task2, task1, task0]) `shouldBe` ref

        it "should return AppState correctly activated even from mid term" $ do

            let ref = Just (task2, [task3, task4], [task1, task0])

            activateTask "2" (task0, [task1, task2, task3, task4], []) `shouldBe` ref
            activateTask "2" (task1, [task2, task3, task4], [task0])   `shouldBe` ref
            activateTask "2" (task2, [task3, task4], [task1, task0])   `shouldBe` ref
            activateTask "2" (task3, [task4], [task2, task1, task0])   `shouldBe` ref
            activateTask "2" (task4, [], [task3, task2, task1, task0])  `shouldBe` ref

        it "should return AppState correctly activated even from end" $ do

            let ref = Just (task4, [], [task3, task2, task1, task0])

            activateTask "4" (task0, [task1, task2, task3, task4], []) `shouldBe` ref
            activateTask "4" (task1, [task2, task3, task4], [task0])   `shouldBe` ref
            activateTask "4" (task2, [task3, task4], [task1, task0])   `shouldBe` ref
            activateTask "4" (task3, [task4], [task2, task1, task0])   `shouldBe` ref
            activateTask "4" (task4, [], [task3, task2, task1, task0]) `shouldBe` ref

        it "should stick to activated already" $ do

            activateTask "0" (task0, [], []) `shouldBe` Just (task0, [], [])
            activateTask "0" (task0, [task1], []) `shouldBe` Just (task0, [task1], [])

        it "should fail it task doesn't exists" $ do

            activateTask "5" (task0, [task1, task2, task3, task4], []) `shouldBe` Nothing
            activateTask "5" (task1, [task2, task3, task4], [task0])   `shouldBe` Nothing
            activateTask "5" (task2, [task3, task4], [task1, task0])   `shouldBe` Nothing
            activateTask "5" (task3, [task4], [task2, task1, task0])   `shouldBe` Nothing
            activateTask "5" (task4, [], [task3, task2, task1, task0]) `shouldBe` Nothing
            activateTask "3" (task0, [task1], []) `shouldBe` Nothing

    describe "read . show" $ do

        it "should be an identity" $ do
            
            timeStamp <- getLocalTime
            let task t = Task { 
                      tag = t
                    , description = "details" ++ t
                    , history = [(timeStamp, Start)] 
                }
                appState = (task "two", [task "three", task "four"], [task "one", task "zero"])
                newAppState = read (show appState) :: AppState
    
            newAppState `shouldBe` appState
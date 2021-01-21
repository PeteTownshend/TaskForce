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

    describe "addTask" $ do

        it "should add at active position" $ do

            addTask task0 ([task1, task2, task3, task4], []) `shouldBe` Just ([task1, task2, task3, task4], [task0])
            addTask task0 ([task2, task3, task4], [task1])   `shouldBe` Just ([task2, task3, task4], [task0, task1])
            addTask task0 ([task3, task4], [task2, task1])   `shouldBe` Just ([task3, task4], [task0, task2, task1])
            addTask task0 ([task4], [task3, task2, task1])   `shouldBe` Just ([task4], [task0, task3, task2, task1])
            addTask task0 ([], [task4, task3, task2, task1]) `shouldBe` Just ([], [task0, task4, task3, task2, task1])
            addTask task2 ([task1, task2, task3, task4], []) `shouldBe` Nothing
            addTask task2 ([task2, task3, task4], [task1])   `shouldBe` Nothing
            addTask task2 ([task3, task4], [task2, task1])   `shouldBe` Nothing
            addTask task2 ([task4], [task3, task2, task1])   `shouldBe` Nothing
            addTask task2 ([], [task4, task3, task2, task1]) `shouldBe` Nothing
      
    describe "activateTask" $ do

        it "should return AppState correctly activated" $ do

            let ref = Just ([task1, task2, task3, task4], [task0])

            activateTask "0" ([task0, task1, task2, task3, task4], []) `shouldBe` ref
            activateTask "0" ([task1, task2, task3, task4], [task0])   `shouldBe` ref
            activateTask "0" ([task2, task3, task4], [task1, task0])   `shouldBe` ref
            activateTask "0" ([task3, task4], [task2, task1, task0])   `shouldBe` ref
            activateTask "0" ([task4], [task3, task2, task1, task0])   `shouldBe` ref
            activateTask "0" ([], [task4, task3, task2, task1, task0]) `shouldBe` ref

        it "should return AppState correctly activated even from mid term" $ do

            let ref = Just ([task3, task4], [task2, task1, task0])

            activateTask "2" ([task0, task1, task2, task3, task4], []) `shouldBe` ref
            activateTask "2" ([task1, task2, task3, task4], [task0])   `shouldBe` ref
            activateTask "2" ([task2, task3, task4], [task1, task0])   `shouldBe` ref
            activateTask "2" ([task3, task4], [task2, task1, task0])   `shouldBe` ref
            activateTask "2" ([task4], [task3, task2, task1, task0])   `shouldBe` ref
            activateTask "2" ([], [task4, task3, task2, task1, task0]) `shouldBe` ref

        it "should return AppState correctly activated even from end" $ do

            let ref = Just ([], [task4, task3, task2, task1, task0])

            activateTask "4" ([task0, task1, task2, task3, task4], []) `shouldBe` ref
            activateTask "4" ([task1, task2, task3, task4], [task0])   `shouldBe` ref
            activateTask "4" ([task2, task3, task4], [task1, task0])   `shouldBe` ref
            activateTask "4" ([task3, task4], [task2, task1, task0])   `shouldBe` ref
            activateTask "4" ([task4], [task3, task2, task1, task0])   `shouldBe` ref
            activateTask "4" ([], [task4, task3, task2, task1, task0]) `shouldBe` ref

        it "should stick to activated already" $ do

            activateTask "0" ([], [task0]) `shouldBe` Just ([], [task0])
            activateTask "0" ([task1], [task0]) `shouldBe` Just ([task1], [task0])

        it "should fail it task doesn't exists" $ do

            activateTask "5" ([task0, task1, task2, task3, task4], []) `shouldBe` Nothing
            activateTask "5" ([task1, task2, task3, task4], [task0])   `shouldBe` Nothing
            activateTask "5" ([task2, task3, task4], [task1, task0])   `shouldBe` Nothing
            activateTask "5" ([task3, task4], [task2, task1, task0])   `shouldBe` Nothing
            activateTask "5" ([task4], [task3, task2, task1, task0])   `shouldBe` Nothing
            activateTask "5" ([], [task4, task3, task2, task1, task0]) `shouldBe` Nothing
            activateTask "3" ([task1], [task0]) `shouldBe` Nothing

    describe "read . show" $ do

        it "should be an identity" $ do
            
            timeStamp <- getLocalTime
            let task t = Task { 
                      tag = t
                    , description = "details" ++ t
                    , history = [(timeStamp, Start)] 
                }
                appState = ([task "three", task "four"], [task "two", task "one", task "zero"])
                newAppState = read (show appState) :: AppState
    
            newAppState `shouldBe` appState

    describe "an empty application state" $ do

        it "should roundtrip as well" $ do

            let newAppState = read (show empty) :: AppState
    
            newAppState `shouldBe` empty

        it "should serialize" $ do
    
            show empty `shouldBe` "([],[])"
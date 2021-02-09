module TaskSpec where

import Test.Hspec ( Spec, describe, it, shouldBe )
import Task ( Task(..), Event(Start, End), updateTaskHistory )
import StateM (getLocalTime)

spec :: Spec
spec = do

    describe "read . show" $ do

        it "should be an identity" $ do
            
            timeStamp <- getLocalTime
            let task = Task { 
                  tag = "one"
                , description = "details"
                , history = [(timeStamp, Start)] 
                }
                task' = read (show task) :: Task
            
            task' `shouldBe` task

    describe "updateTaskHistory" $ do

        it "should update a task history" $ do

            timeStamp1 <- getLocalTime
            timeStamp2 <- getLocalTime
            let task = Task { 
                  tag = "one"
                , description = "details"
                , history = [(timeStamp1, Start)] 
                }
                task' = updateTaskHistory timeStamp2 End task
                task'' = Task { 
                  tag = "one"
                , description = "details"
                , history = [
                      (timeStamp2, End)
                    , (timeStamp1, Start)
                    ] 
                }

            task' `shouldBe` task''
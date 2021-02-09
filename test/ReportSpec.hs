module ReportSpec where

import Test.Hspec ( Spec, describe, it, shouldBe )
import Task ( Event(Created, End, Log, Start) )
import StateM (getLocalTime)
import Report ( consumption, getMinutes )
import Data.Time (LocalTime)

spec :: Spec
spec = do

    let t1 = read "2021-01-24 15:45:09.5877978" :: LocalTime
        t2 = read "2021-01-24 13:39:29.2659037" :: LocalTime
        t3 = read "2021-01-22 22:47:08.8190806" :: LocalTime
        t4 = read "2021-01-22 22:37:53.3652561" :: LocalTime
        t5 = read "2021-01-22 22:37:48.3287209" :: LocalTime
        t6 = read "2021-01-22 22:35:28.9398812" :: LocalTime
        t7 = read "2021-01-22 22:10:34.3475510" :: LocalTime
        t8 = read "2021-01-22 22:10:11.8760891" :: LocalTime

    describe "getMinutes" $ do

        it "should calculate the minutes between" $ do

            getMinutes t2 t1 `shouldBe` 125
            getMinutes t4 t3 `shouldBe` 9
            getMinutes t7 t5 `shouldBe` 27

    describe "consumption" $ do

        it "should calculate a simple one" $ do

            consumption [(t1, End), (t2, Start)] `shouldBe` (2, 5)

        it "should analyse efforts" $ do

            let history = [
                    (t1, End), (t2, Start), (t3, End), (t4, Start),
                    (t5, End), (t6, Log "hey"), (t7,  Start), (t8, Created)
                    ]

            consumption history `shouldBe` (2, 41)
            
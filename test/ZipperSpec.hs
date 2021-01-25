module ZipperSpec where

import Test.Hspec
import Zipper

spec :: Spec
spec = do

    describe "toList" $ do

        let list = [0, 1, 2, 3, 4]

        it "lists" $ do

            toList ([0, 1, 2, 3, 4], []) `shouldBe` list
            toList ([1, 2, 3, 4], [0])   `shouldBe` list
            toList ([2, 3, 4], [1, 0])   `shouldBe` list
            toList ([3, 4], [2, 1, 0])   `shouldBe` list
            toList ([4], [3, 2, 1, 0])   `shouldBe` list
            toList ([], [4, 3, 2, 1, 0]) `shouldBe` list
    
    describe "slideUp" $ do

        it "should slide up" $ do
            
            slideUp ([0, 1, 2, 3, 4], []) `shouldBe` Just ([1, 2, 3, 4], [0])
            slideUp ([1, 2, 3, 4], [0])   `shouldBe` Just ([2, 3, 4], [1, 0])
            slideUp ([2, 3, 4], [1, 0])   `shouldBe` Just ([3, 4], [2, 1, 0])
            slideUp ([3, 4], [2, 1, 0])   `shouldBe` Just ([4], [3, 2, 1, 0])
            slideUp ([4], [3, 2, 1, 0])   `shouldBe` Just ([], [4, 3, 2, 1, 0])
            slideUp ([], [4, 3, 2, 1, 0]) `shouldBe` Nothing

    describe "slideDown" $ do

        it "should slide down" $ do
            
            slideDown ([], [4, 3, 2, 1, 0]) `shouldBe` Just ([4], [3, 2, 1, 0])
            slideDown ([4], [3, 2, 1, 0])   `shouldBe` Just ([3, 4], [2, 1, 0])
            slideDown ([3, 4], [2, 1, 0])   `shouldBe` Just ([2, 3, 4], [1, 0])
            slideDown ([2, 3, 4], [1, 0])   `shouldBe` Just ([1, 2, 3, 4], [0])
            slideDown ([1, 2, 3, 4], [0])   `shouldBe` Just ([0, 1, 2, 3, 4], [])
            slideDown ([0, 1, 2, 3, 4], []) `shouldBe` Nothing

    describe "open" $ do

        let fullyOpened = ([0, 1, 2, 3, 4], [])

        it "should slide down as much as possible" $ do

            open ([], [4, 3, 2, 1, 0]) `shouldBe` fullyOpened
            open ([4], [3, 2, 1, 0])   `shouldBe` fullyOpened
            open ([3, 4], [2, 1, 0])   `shouldBe` fullyOpened
            open ([2, 3, 4], [1, 0])   `shouldBe` fullyOpened
            open ([1, 2, 3, 4], [0])   `shouldBe` fullyOpened
            open ([0, 1, 2, 3, 4], []) `shouldBe` fullyOpened

    describe "exists" $ do

        it "should return True if exists already" $ do

            exists ((==) 0) ([0, 1, 2, 3, 4], []) `shouldBe` True
            exists ((==) 0) ([1, 2, 3, 4], [0])   `shouldBe` True
            exists ((==) 0) ([2, 3, 4], [1, 0])   `shouldBe` True
            exists ((==) 0) ([3, 4], [2, 1, 0])   `shouldBe` True
            exists ((==) 0) ([4], [3, 2, 1, 0])   `shouldBe` True
            exists ((==) 0) ([], [4, 3, 2, 1, 0]) `shouldBe` True
            exists ((==) 2) ([0, 1, 2, 3, 4], []) `shouldBe` True
            exists ((==) 2) ([1, 2, 3, 4], [0])   `shouldBe` True
            exists ((==) 2) ([2, 3, 4], [1, 0])   `shouldBe` True
            exists ((==) 2) ([3, 4], [2, 1, 0])   `shouldBe` True
            exists ((==) 2) ([4], [3, 2, 1, 0])   `shouldBe` True
            exists ((==) 2) ([], [4, 3, 2, 1, 0]) `shouldBe` True
            exists ((==) 4) ([0, 1, 2, 3, 4], []) `shouldBe` True
            exists ((==) 4) ([1, 2, 3, 4], [0])   `shouldBe` True
            exists ((==) 4) ([2, 3, 4], [1, 0])   `shouldBe` True
            exists ((==) 4) ([3, 4], [2, 1, 0])   `shouldBe` True
            exists ((==) 4) ([4], [3, 2, 1, 0])   `shouldBe` True
            exists ((==) 4) ([], [4, 3, 2, 1, 0]) `shouldBe` True

        it "should return False if a  doesn't exist" $ do

            exists ((==) 5) ([0, 1, 2, 3, 4], []) `shouldBe` False
            exists ((==) 5) ([1, 2, 3, 4], [0])   `shouldBe` False
            exists ((==) 5) ([2, 3, 4], [1, 0])   `shouldBe` False
            exists ((==) 5) ([3, 4], [2, 1, 0])   `shouldBe` False
            exists ((==) 5) ([4], [3, 2, 1, 0])   `shouldBe` False
            exists ((==) 5) ([], [4, 3, 2, 1, 0]) `shouldBe` False
            exists ((==) 5) ([], [])              `shouldBe` False

    describe "prune" $ do

        it "should filter valid elements" $ do

            prune ((==) 0) ([0, 1, 2, 3, 4], []) `shouldBe` ([], [0])
            prune ((==) 0) ([1, 2, 3, 4], [0])   `shouldBe` ([], [0])
            prune ((==) 0) ([2, 3, 4], [1, 0])   `shouldBe` ([], [0])
            prune ((==) 0) ([3, 4], [2, 1, 0])   `shouldBe` ([], [0])
            prune ((==) 0) ([4], [3, 2, 1, 0])   `shouldBe` ([], [0])
            prune ((==) 0) ([], [4, 3, 2, 1, 0]) `shouldBe` ([], [0])

            prune (\i -> mod i 2 == 0) ([0, 1, 2, 3, 4], []) `shouldBe` ([], [4, 2, 0])
            prune (\i -> mod i 2 == 0) ([1, 2, 3, 4], [0])   `shouldBe` ([], [4, 2, 0])
            prune (\i -> mod i 2 == 0) ([2, 3, 4], [1, 0])   `shouldBe` ([], [4, 2, 0])
            prune (\i -> mod i 2 == 0) ([3, 4], [2, 1, 0])   `shouldBe` ([], [4, 2, 0])
            prune (\i -> mod i 2 == 0) ([4], [3, 2, 1, 0])   `shouldBe` ([], [4, 2, 0])
            prune (\i -> mod i 2 == 0) ([], [4, 3, 2, 1, 0]) `shouldBe` ([], [4, 2, 0])
            prune (\i -> mod i 2 == 0) ([], [])              `shouldBe` ([], [])
            
            prune (const True) ([0, 1, 2, 3, 4], []) `shouldBe` ([], [4, 3, 2, 1, 0])
            prune (const True) ([1, 2, 3, 4], [0])   `shouldBe` ([], [4, 3, 2, 1, 0])
            prune (const True) ([2, 3, 4], [1, 0])   `shouldBe` ([], [4, 3, 2, 1, 0])
            prune (const True) ([3, 4], [2, 1, 0])   `shouldBe` ([], [4, 3, 2, 1, 0])
            prune (const True) ([4], [3, 2, 1, 0])   `shouldBe` ([], [4, 3, 2, 1, 0])
            prune (const True) ([], [4, 3, 2, 1, 0]) `shouldBe` ([], [4, 3, 2, 1, 0])

            prune (const False) ([0, 1, 2, 3, 4], []) `shouldBe` ([], [])
            prune (const False) ([2, 3, 4], [1, 0])   `shouldBe` ([], [])
            prune (const False) ([3, 4], [2, 1, 0])   `shouldBe` ([], [])
            prune (const False) ([4], [3, 2, 1, 0])   `shouldBe` ([], [])
            prune (const False) ([], [4, 3, 2, 1, 0]) `shouldBe` ([], [])

    describe "slideTo" $ do

        it "should slide zipper to first match" $ do

            let ref = Just ([1, 2, 3, 4], [0])

            slideTo ((==) 0) ([0, 1, 2, 3, 4], []) `shouldBe` ref
            slideTo ((==) 0) ([1, 2, 3, 4], [0])   `shouldBe` ref
            slideTo ((==) 0) ([2, 3, 4], [1, 0])   `shouldBe` ref
            slideTo ((==) 0) ([3, 4], [2, 1, 0])   `shouldBe` ref
            slideTo ((==) 0) ([4], [3, 2, 1, 0])   `shouldBe` ref
            slideTo ((==) 0) ([], [4, 3, 2, 1, 0]) `shouldBe` ref

        it "should slide zipper to first match even from mid" $ do

            let ref = Just ([3, 4], [2, 1, 0])

            slideTo ((==) 2) ([0, 1, 2, 3, 4], []) `shouldBe` ref
            slideTo ((==) 2) ([1, 2, 3, 4], [0])   `shouldBe` ref
            slideTo ((==) 2) ([2, 3, 4], [1, 0])   `shouldBe` ref
            slideTo ((==) 2) ([3, 4], [2, 1, 0])   `shouldBe` ref
            slideTo ((==) 2) ([4], [3, 2, 1, 0])   `shouldBe` ref
            slideTo ((==) 2) ([], [4, 3, 2, 1, 0]) `shouldBe` ref

        it "should slide zipper to first match even from end" $ do

            let ref = Just ([], [4, 3, 2, 1, 0])

            slideTo ((==) 4) ([0, 1, 2, 3, 4], []) `shouldBe` ref
            slideTo ((==) 4) ([1, 2, 3, 4], [0])   `shouldBe` ref
            slideTo ((==) 4) ([2, 3, 4], [1, 0])   `shouldBe` ref
            slideTo ((==) 4) ([3, 4], [2, 1, 0])   `shouldBe` ref
            slideTo ((==) 4) ([4], [3, 2, 1, 0])   `shouldBe` ref
            slideTo ((==) 4) ([], [4, 3, 2, 1, 0]) `shouldBe` ref

        it "should stick to slided already" $ do

            slideTo ((==) 0) ([0], [])    `shouldBe` Just ([], [0])
            slideTo ((==) 0) ([0, 1], []) `shouldBe` Just ([1], [0])

        it "should fail if it doesn't exists" $ do

            let ref = Nothing

            slideTo ((==) 5) ([0, 1, 2, 3, 4], []) `shouldBe` ref
            slideTo ((==) 5) ([1, 2, 3, 4], [0])   `shouldBe` ref
            slideTo ((==) 5) ([2, 3, 4], [1, 0])   `shouldBe` ref
            slideTo ((==) 5) ([3, 4], [2, 1, 0])   `shouldBe` ref
            slideTo ((==) 5) ([4], [3, 2, 1, 0])   `shouldBe` ref
            slideTo ((==) 5) ([], [4, 3, 2, 1, 0]) `shouldBe` ref
            slideTo ((==) 3) ([0, 1], [])          `shouldBe` ref

    describe "modify" $ do

        it "should change the slider" $ do

            let ref = ([3, 4], [5, 1, 0])
                inp = ([3, 4], [2, 1, 0])

            modify (const 5) inp `shouldBe` ref

    describe "add" $ do

        it "should add at slider position" $ do

            add (==) 0 ([1, 2, 3, 4], []) `shouldBe` Just ([1, 2, 3, 4], [0])
            add (==) 0 ([2, 3, 4], [1])   `shouldBe` Just ([2, 3, 4], [0, 1])
            add (==) 0 ([3, 4], [2, 1])   `shouldBe` Just ([3, 4], [0, 2, 1])
            add (==) 0 ([4], [3, 2, 1])   `shouldBe` Just ([4], [0, 3, 2, 1])
            add (==) 0 ([], [4, 3, 2, 1]) `shouldBe` Just ([], [0, 4, 3, 2, 1])
            add (==) 2 ([1, 2, 3, 4], []) `shouldBe` Nothing
            add (==) 2 ([2, 3, 4], [1])   `shouldBe` Nothing
            add (==) 2 ([3, 4], [2, 1])   `shouldBe` Nothing
            add (==) 2 ([4], [3, 2, 1])   `shouldBe` Nothing
            add (==) 2 ([], [4, 3, 2, 1]) `shouldBe` Nothing

    describe "read . show" $ do

        it "should be an identity" $ do
            
            let zipper = ([3, 4], [2, 1, 0])
                newZipper = read (show zipper) :: Zipper Int
    
            newZipper `shouldBe` zipper
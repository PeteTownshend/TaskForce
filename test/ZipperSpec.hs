module ZipperSpec where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Zipper
    ( add,
      exists,
      modify,
      open,
      prune,
      slideDown,
      slideTo,
      slideUp,
      toList,
      Zipper(Zipper, Empty) )

spec :: Spec
spec = do

    let empty = Empty :: Zipper Int

    describe "toList" $ do

        it "lists" $ do

            toList empty                          `shouldBe` []
            toList (Zipper [] 0 [])               `shouldBe` [0]
            toList (Zipper [] 1 [0])              `shouldBe` [1, 0]
            toList (Zipper [] 2 [1, 0])           `shouldBe` [2, 1, 0]
            toList (Zipper [] 3 [2, 1, 0])        `shouldBe` [3, 2, 1, 0]
            toList (Zipper [] 4 [3, 2, 1, 0])     `shouldBe` [4, 3, 2, 1, 0]
            toList (Zipper [5] 4 [3, 2, 1, 0])    `shouldBe` [4, 3, 2, 1, 0, 5]
            toList (Zipper [5, 6] 4 [3, 2, 1, 0]) `shouldBe` [4, 3, 2, 1, 0, 5, 6]
    
    describe "slideUp" $ do

        it "should slide up" $ do
            
            slideUp empty                      `shouldBe` empty
            slideUp (Zipper [] 0 [])           `shouldBe` Zipper [] 0 []
            slideUp (Zipper [1, 2, 3, 4] 0 []) `shouldBe` Zipper [2, 3, 4] 1 [0]
            slideUp (Zipper [2, 3, 4] 1 [0])   `shouldBe` Zipper [3, 4] 2 [1, 0]
            slideUp (Zipper [3, 4] 2 [1, 0])   `shouldBe` Zipper [4] 3 [2, 1, 0]
            slideUp (Zipper [4] 3 [2, 1, 0])   `shouldBe` Zipper [] 4 [3, 2, 1, 0]
            slideUp (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` Zipper [] 4 [3, 2, 1, 0]

    describe "slideDown" $ do

        it "should slide down" $ do
            
            slideDown empty                      `shouldBe` empty
            slideDown (Zipper [] 0 [])           `shouldBe` Zipper [] 0 []
            slideDown (Zipper [1, 2, 3, 4] 0 []) `shouldBe` Zipper [1, 2, 3, 4] 0 []
            slideDown (Zipper [2, 3, 4] 1 [0])   `shouldBe` Zipper [1, 2, 3, 4] 0 []
            slideDown (Zipper [3, 4] 2 [1, 0])   `shouldBe` Zipper [2, 3, 4] 1 [0]
            slideDown (Zipper [4] 3 [2, 1, 0])   `shouldBe` Zipper [3, 4] 2 [1, 0]
            slideDown (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` Zipper [4] 3 [2, 1, 0]

    describe "open" $ do

        let fullyOpened = Zipper [] 4 [3, 2, 1, 0]

        it "should slide down as much as possible" $ do

            open empty                      `shouldBe` empty
            open (Zipper [] 0 [])           `shouldBe` Zipper [] 0 []
            open (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` fullyOpened
            open (Zipper [4] 3 [2, 1, 0])   `shouldBe` fullyOpened
            open (Zipper [3, 4] 2 [1, 0])   `shouldBe` fullyOpened
            open (Zipper [2, 3, 4] 1 [0])   `shouldBe` fullyOpened
            open (Zipper [1, 2, 3, 4] 0 []) `shouldBe` fullyOpened

    describe "exists" $ do

        it "should return True if exists already" $ do

            exists (0 ==) (Zipper [] 0 [])           `shouldBe` True
            exists (0 ==) (Zipper [1, 2, 3, 4] 0 []) `shouldBe` True
            exists (0 ==) (Zipper [2, 3, 4] 1 [0])   `shouldBe` True
            exists (0 ==) (Zipper [3, 4] 2 [1, 0])   `shouldBe` True
            exists (0 ==) (Zipper [4] 3 [2, 1, 0])   `shouldBe` True
            exists (0 ==) (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` True
            exists (2 ==) (Zipper [1, 2, 3, 4] 0 []) `shouldBe` True
            exists (2 ==) (Zipper [2, 3, 4] 1 [0])   `shouldBe` True
            exists (2 ==) (Zipper [3, 4] 2 [1, 0])   `shouldBe` True
            exists (2 ==) (Zipper [4] 3 [2, 1, 0])   `shouldBe` True
            exists (2 ==) (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` True
            exists (4 ==) (Zipper [1, 2, 3, 4] 0 []) `shouldBe` True
            exists (4 ==) (Zipper [2, 3, 4] 1 [0])   `shouldBe` True
            exists (4 ==) (Zipper [3, 4] 2 [1, 0])   `shouldBe` True
            exists (4 ==) (Zipper [4] 3 [2, 1, 0])   `shouldBe` True
            exists (4 ==) (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` True

        it "should return False if a  doesn't exist" $ do

            exists (5 ==) empty                      `shouldBe` False
            exists (5 ==) (Zipper [] 0 [])           `shouldBe` False
            exists (5 ==) (Zipper [1, 2, 3, 4] 0 []) `shouldBe` False
            exists (5 ==) (Zipper [2, 3, 4] 1 [0])   `shouldBe` False
            exists (5 ==) (Zipper [3, 4] 2 [1, 0])   `shouldBe` False
            exists (5 ==) (Zipper [4] 3 [2, 1, 0])   `shouldBe` False
            exists (5 ==) (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` False

    describe "prune" $ do

        let single = Zipper [] 0 []
            
        it "should filter valid elements" $ do

            prune (0 ==) (Zipper [1, 2, 3, 4] 0 [])  `shouldBe` single
            prune (0 ==) (Zipper [2, 3, 4] 1 [0])    `shouldBe` single
            prune (0 ==) (Zipper [3, 4] 2 [1, 0])    `shouldBe` single
            prune (0 ==) (Zipper [4] 3 [3, 2, 1, 0]) `shouldBe` single
            prune (0 ==) (Zipper [] 4 [3, 2, 1, 0])  `shouldBe` single

            prune even (Zipper [1, 2, 3, 4] 0 []) `shouldBe` Zipper [2, 4] 0 []
            prune even (Zipper [2, 3, 4] 1 [0])   `shouldBe` Zipper [2, 4] 0 []
            prune even (Zipper [3, 4] 2 [1, 0])   `shouldBe` Zipper [4] 2 [0]
            prune even (Zipper [4] 3 [2, 1, 0])   `shouldBe` Zipper [4] 2 [0]
            prune even (Zipper [] 3 [2, 1, 0])    `shouldBe` Zipper [] 2 [0]
            prune even (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` Zipper [] 4 [2, 0]
            prune even empty                      `shouldBe` empty
            
            prune (const True) (Zipper [1, 2, 3, 4] 0 []) `shouldBe` Zipper [1, 2, 3, 4] 0 []
            prune (const True) (Zipper [2, 3, 4] 1 [0])   `shouldBe` Zipper [2, 3, 4] 1 [0]
            prune (const True) (Zipper [3, 4] 2 [1, 0])   `shouldBe` Zipper [3, 4] 2 [1, 0]
            prune (const True) (Zipper [4] 3 [2, 1, 0])   `shouldBe` Zipper [4] 3 [2, 1, 0]
            prune (const True) (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` Zipper [] 4 [3, 2, 1, 0]

            prune (const False) (Zipper [1, 2, 3, 4] 0 []) `shouldBe` empty
            prune (const False) (Zipper [2, 3, 4] 1 [0])   `shouldBe` empty
            prune (const False) (Zipper [3, 4] 2 [1, 0])   `shouldBe` empty
            prune (const False) (Zipper [4] 3 [2, 1, 0])   `shouldBe` empty
            prune (const False) (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` empty

    describe "slideTo" $ do

        it "should slide zipper to first match" $ do

            let ref = Just (Zipper [1, 2, 3, 4] 0 [])

            slideTo (0 ==) (Zipper [1, 2, 3, 4] 0 []) `shouldBe` ref
            slideTo (0 ==) (Zipper [2, 3, 4] 1 [0])   `shouldBe` ref
            slideTo (0 ==) (Zipper [3, 4] 2 [1, 0])   `shouldBe` ref
            slideTo (0 ==) (Zipper [4] 3 [2, 1, 0])   `shouldBe` ref
            slideTo (0 ==) (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` ref
            slideTo (0 ==) empty                      `shouldBe` Nothing

        it "should slide zipper to first match even from mid" $ do

            let ref = Just (Zipper [3, 4] 2 [1, 0])

            slideTo (2 ==) (Zipper [1, 2, 3, 4] 0 []) `shouldBe` ref
            slideTo (2 ==) (Zipper [2, 3, 4] 1 [0])   `shouldBe` ref
            slideTo (2 ==) (Zipper [3, 4] 2 [1, 0])   `shouldBe` ref
            slideTo (2 ==) (Zipper [4] 3 [2, 1, 0])   `shouldBe` ref
            slideTo (2 ==) (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` ref

        it "should slide zipper to first match even from end" $ do

            let ref = Just (Zipper [] 4 [3, 2, 1, 0])

            slideTo (4 ==) (Zipper [1, 2, 3, 4] 0 []) `shouldBe` ref
            slideTo (4 ==) (Zipper [2, 3, 4] 1 [0])   `shouldBe` ref
            slideTo (4 ==) (Zipper [3, 4] 2 [1, 0])   `shouldBe` ref
            slideTo (4 ==) (Zipper [4] 3 [2, 1, 0])   `shouldBe` ref
            slideTo (4 ==) (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` ref

        it "should stick to slided already" $ do

            slideTo (0 ==) (Zipper [] 0 [])      `shouldBe` Just (Zipper [] 0 [])
            slideTo (0 ==) (Zipper [1] 0 [2, 3]) `shouldBe` Just (Zipper [1] 0 [2, 3])

        it "should fail if it doesn't exists" $ do

            slideTo (5 ==) (Zipper [1, 2, 3, 4] 0 []) `shouldBe` Nothing
            slideTo (5 ==) (Zipper [2, 3, 4] 1 [0])   `shouldBe` Nothing
            slideTo (5 ==) (Zipper [3, 4] 2 [1, 0])   `shouldBe` Nothing
            slideTo (5 ==) (Zipper [4] 3 [2, 1, 0])   `shouldBe` Nothing
            slideTo (5 ==) (Zipper [] 4 [3, 2, 1, 0]) `shouldBe` Nothing
            slideTo (3 ==) (Zipper [1] 0 [])          `shouldBe` Nothing

    describe "modify" $ do

        it "should change the slider" $ do

            let ref = Zipper [3, 4] 5 [1, 0]
                inp = Zipper [3, 4] 2 [1, 0]

            modify (const 5) inp `shouldBe` ref

    describe "add" $ do

        it "should add at slider position" $ do

            add (==) 0 empty                   `shouldBe` Zipper [] 0 []
            add (==) 0 (Zipper [2, 3, 4] 1 []) `shouldBe` Zipper [2, 3, 4] 0 [1]
            add (==) 0 (Zipper [3, 4] 2 [1])   `shouldBe` Zipper [3, 4] 0 [2, 1]
            add (==) 0 (Zipper [4] 3 [2, 1])   `shouldBe` Zipper [4] 0 [3, 2, 1]
            add (==) 0 (Zipper [] 4 [3, 2, 1]) `shouldBe` Zipper [] 0 [4, 3, 2, 1]
            add (==) 2 (Zipper [2, 3, 4] 1 []) `shouldBe` Zipper [2, 3, 4] 1 []
            add (==) 2 (Zipper [3, 4] 2 [1])   `shouldBe` Zipper [3, 4] 2 [1]
            add (==) 2 (Zipper [4] 3 [2, 1])   `shouldBe` Zipper [4] 3 [2, 1]
            add (==) 2 (Zipper [] 4 [3, 2, 1]) `shouldBe` Zipper [] 4 [3, 2, 1]

    describe "read . show" $ do

        it "should be an identity" $ do
            
            let zipper = Zipper [3, 4] 2 [1, 0]
                newZipper = read (show zipper) :: Zipper Int
    
            newZipper `shouldBe` zipper

        it "should be an identity for an empty zipper as well" $ do
            
            let newEmpty = read (show empty) :: Zipper Int
    
            newEmpty `shouldBe` empty
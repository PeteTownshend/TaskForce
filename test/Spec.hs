import Test.Hspec

main :: IO ()
main = hspec $ do

    describe "fake" $ do
        it "should return something" $ do
            length [] `shouldBe` 0

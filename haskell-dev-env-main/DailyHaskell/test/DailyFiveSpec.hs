module DailyFiveSpec where

import Test.Hspec
import DailyFive

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "multPairs" $ do
        context "multPairs [(1,2), (3,4), (5,6)]" $
            it "should be [2, 12, 30]" $
                (multPairs [(1,2), (3,4), (5,6)]) `shouldBe` [2, 12, 30]
        
        context "multPairs []" $
            it "should be []" $
                (multPairs []) `shouldBe` []
        
        context "multPairs [(2,3), (0,5)]" $
            it "should be [6, 0]" $
                (multPairs [(2,3), (0,5)]) `shouldBe` [6, 0]


    describe "squareList" $ do
        context "squareList [1,3,2]" $
            it "should be [(1,1), (3, 9), (2, 4)]" $
                (squareList [1, 3, 2]) `shouldBe` [(1,1), (3, 9), (2, 4)]
        
        context "squareList []" $
            it "should be []" $
                (squareList []) `shouldBe` []

        context "squareList [0]" $
            it "should be [(0,0)]" $
                (squareList [0]) `shouldBe` [(0,0)]


    describe "findLowercase" $ do
        context "findLowercase [\"test\", \"Test\"]" $
            it "should be [True, False]" $
                (findLowercase ["test", "Test"]) `shouldBe` [True, False]
        
        context "findLowercase [\"tEST\"]" $
            it "should be [True]" $
                (findLowercase ["tEST"]) `shouldBe` [True]
        
        context "findLowercase []" $
            it "should be []" $
                (findLowercase []) `shouldBe` []

    -- describe "temp" $ do
    --     context "temp " $
    --         it "should be " $
    --             (temp ) `shouldBe` 
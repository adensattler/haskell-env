module DailyTwoSpec where

import Test.Hspec
import Control.Exception (evaluate)
import DailyTwo

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "every4th" $ do
        context " when given a list [1..10]" $ do
            it "produces the correct result" $
                every4th [1..10] `shouldBe` [4, 8]
        
        context "every4th [1..4]" $ do
            it "produces [4]" $
                every4th [1..4] `shouldBe` [4]
        
        context "every4th [1..3]" $ do
            it "produces []" $
                every4th [1..3] `shouldBe` []

    describe "tupleDotProduct" $ do
        context "tupleDotProduct [] []" $ do
            it "should be 0" $
                tupleDotProduct [] [] `shouldBe` 0

        context "when given lists of equal length" $ do
            it "produces the correct dot product" $
                tupleDotProduct [1,2,3] [4,5,6] `shouldBe` 32

        context "when given lists of different lengths" $ do
            it "throws an error" $
                evaluate (tupleDotProduct [1,2,3] [4,5]) `shouldThrow` anyErrorCall

    describe "appendToEach" $ do
        context "appendToEach '!!!' ['hello', 'world']" $ do
            it "should be ['hello!', 'world!']" $
                appendToEach "!!!" ["hello", "world"] `shouldBe` ["hello!!!", "world!!!"]
        
        context "appendToEach '!!!' []" $ do
            it "should be []" $
                appendToEach "!!!" [] `shouldBe` []

        context "appendToEach '' ['hello', 'world']" $ do
            it "should be ['hello', 'world']" $
                appendToEach "" ["hello", "world"] `shouldBe` ["hello", "world"]

    describe "toSetList" $ do
        context "toSetList []" $ do
            it "should be []" $
                (toSetList [] :: [Int]) `shouldBe` []
        
        context "toSetList [5, 1, 2, 3, 3, 4, 5, 5]" $ do
            it "should be [1, 2, 3, 4, 5]" $
                toSetList [5, 1, 2, 3, 3, 4, 5, 5] `shouldBe` [1, 2, 3, 4, 5]
        
        context "toSetList [1, 2, 3, 4, 5]" $ do
            it "should be [1, 2, 3, 4, 5]" $
                toSetList [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5]

    describe "contains" $ do
        context "contains 2 [1, 2, 3]" $ do
            it "returns True" $
                contains 2 [1, 2, 3] `shouldBe` True

        context "contains 4 [1, 2, 3]" $ do
            it "returns False" $
                contains 4 [1, 2, 3] `shouldBe` False

        context "contains _ []" $ do
            it "returns False" $
                contains 4 [] `shouldBe` False
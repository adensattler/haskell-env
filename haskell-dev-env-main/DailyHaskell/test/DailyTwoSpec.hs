module DailyTwoSpec where

import Test.Hspec
import Control.Exception (evaluate)
import DailyTwo

spec :: Spec
spec = do
    describe "every4th" $ do
        it "" $
            every4th [1..10] `shouldBe` [4, 8]

    describe "tupleDotProduct" $ do
        context "when given lists of equal length" $ do
            it "produces the correct dot product" $
                tupleDotProduct [1,2,3] [4,5,6] `shouldBe` 32

        context "when given lists of different lengths" $ do
            it "throws an error" $
                evaluate (tupleDotProduct [1,2,3] [4,5]) `shouldThrow` anyErrorCall

    describe "appendToEach" $ do
        it "appends the string to each element of the list" $
            appendToEach "!" ["hello", "world"] `shouldBe` ["hello!", "world!"]

    describe "toSetList" $ do
        it "produces a list with duplicates removed" $
            toSetList [5, 1, 2, 3, 3, 4, 5, 5] `shouldBe` [1, 2, 3, 4, 5]

    describe "contains" $ do
        context "when the element is present in the list" $ do
            it "returns True" $
                contains 2 [1, 2, 3] `shouldBe` True

        context "when the element is not present in the list" $ do
            it "returns False" $
                contains 4 [1, 2, 3] `shouldBe` False
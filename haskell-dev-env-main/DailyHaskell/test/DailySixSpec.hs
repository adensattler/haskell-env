module DailySixSpec where

import Test.Hspec
import DailySix

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "shorterThan" $ do
        context "shorterThan 3 [\"cat\", \"dog\", \"elephant\"]" $
            it "should be [\"cat\", \"dog\"]" $
                (shorterThan 3 ["cat", "dog", "elephant"]) `shouldBe` ["cat", "dog"]
        
        context "shorterThan 2 [\"apple\", \"banana\", \"orange\"]" $
            it "should be []" $
                (shorterThan 2 ["apple", "banana", "orange"]) `shouldBe` []

        context "shorterThan 7 [\"blue\", \"red\", \"green\"]" $
            it "should be [\"blue\", \"red\", \"green\"]" $
                (shorterThan 7 ["blue", "red", "green"]) `shouldBe` ["blue", "red", "green"]

        context "shorterThan 2 []" $
            it "should be []" $
                (shorterThan 2 []) `shouldBe` []


    describe "removeMultiples" $ do
        context "removeMultiples 5 [3,5,10,9, 15]" $
            it "should be [3,9]" $
                (removeMultiples 5 [3,5,10,9,15]) `shouldBe` [3,9]
        
        context "removeMultiples 7 [1,2,3,4,5]" $
            it "should be [1,2,3,4,5]" $
                (removeMultiples 7 [1,2,3,4,5]) `shouldBe` [1,2,3,4,5]

        context "removeMultiples 3 []" $
            it "should be []" $
                (removeMultiples 3 []) `shouldBe` []
        
        context "removeMultiples 3 [9]" $
            it "should be []" $
                (removeMultiples 3 [9]) `shouldBe` []

    
    describe "onlyJust" $ do
        context "onlyJust onlyJust [Nothing, Just 5, Nothing, Just 10]" $
            it "should be [Just 5, Just 10]" $
                (onlyJust [Nothing, Just 5, Nothing, Just 10]) `shouldBe` [Just 5, Just 10]

        context "onlyJust [Nothing, Nothing, Nothing]" $
            it "should be []" $
                (onlyJust [Nothing, Nothing, Nothing] :: [Maybe Int]) `shouldBe` []

        context "onlyJust onlyJust [Just 5, Just 10]" $
            it "should be [Just 5, Just 10]" $
                (onlyJust [Just 5, Just 10]) `shouldBe` [Just 5, Just 10]

        

        
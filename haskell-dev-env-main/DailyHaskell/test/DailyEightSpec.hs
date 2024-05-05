module DailyEightSpec where

import Test.Hspec
import DailyEight

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "findSmallest" $ do
        context "findSmallest [5, 2, 5, 7, 3]" $
            it "should be 2" $
                (findSmallest [5, 2, 5, 7, 3]) `shouldBe` Just 2
        context "findSmallest []" $
            it "should be Nothing" $
                (findSmallest [] :: Maybe Int) `shouldBe` Nothing
        context "findSmallest [5, 2, 5, 7, 3]" $
            it "should be 2" $
                (findSmallest ['z', 'g', 'b', 'k', 'j']) `shouldBe` Just 'b'

    describe "allTrue" $ do
        context "allTrue [True, True, True]" $
            it "should be True" $
                (allTrue [True, True, True]) `shouldBe` Just True
        context "allTrue [True, False, True]" $
            it "should be False" $
                (allTrue [True, False, True]) `shouldBe` Just False
        context "allTrue []" $
            it "should be Nothing" $
                (allTrue []) `shouldBe` Nothing
        

    describe "countAllVotes" $ do
        context "countAllVotes [True, False, Nothing, True]" $
            it "should be (2, 1, 1)" $
                (countAllVotes [Just True, Just False, Nothing, Just True]) `shouldBe` (2, 1, 1)
        context "countAllVotes []" $
            it "should be (0, 0, 0)" $
                (countAllVotes []) `shouldBe` (0, 0, 0)
        context "countAllVotes [True, Nothing, Nothing, True]" $
            it "should be (2, 0, 2)" $
                (countAllVotes [Just True, Nothing, Nothing, Just True]) `shouldBe` (2, 0, 2)
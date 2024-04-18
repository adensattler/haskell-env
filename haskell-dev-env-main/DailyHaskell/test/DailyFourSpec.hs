module DailyFourSpec where

import Test.Hspec
import DailyFour

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "zip3Lists" $ do
        context "zip3Lists [1, 2, 3] ['a', 'b', 'c'] [4, 5, 6]" $
            it "should be [(1, 'a', 4), (2, 'b', 5), (3, 'c', 6)]" $
                (zip3Lists [1, 2, 3] ['a', 'b', 'c'][4, 5, 6]) `shouldBe` [(1, 'a', 4), (2, 'b', 5), (3, 'c', 6)]

        context "zip3Lists [] ['a', 'b', 'c'][4, 5, 6]" $
            it "should be []" $
                (zip3Lists [] ['a', 'b', 'c'][4, 5, 6]) `shouldBe` ([] :: [(Int, Char, Int)])

        context "zip3Lists [1, 2, 3] [10, 11, 12] [4, 5, 6]" $
            it "should be [(1, 10, 4), (2, 11, 5), (3, 12, 6)]" $
                (zip3Lists [1, 2, 3] [10, 11, 12] [4, 5, 6]) `shouldBe` [(1, 10, 4), (2, 11, 5), (3, 12, 6)]


    describe "unzipTriples" $ do
        context "unzipTriples [ (1,2,3), (4, 5, 6), (7, 8, 9) ]" $
            it "should be " $
                (unzipTriples [ (1,2,3), (4, 5, 6), (7, 8, 9) ]) `shouldBe` ( [1,4,7], [2, 5, 8], [3, 6, 9] )

        context "unzipTriples []" $
            it "should be ([], [], [])" $
                (unzipTriples []) `shouldBe` ([] :: [Int], [] :: [Int], [] :: [Int])
        
        context "unzipTriples [ (1,2,3), (4, 5, 6), (7, 8, 9), (10, 11, 12) ]" $
            it "should be " $
                (unzipTriples [ (1,2,3), (4, 5, 6), (7, 8, 9), (10, 11, 12) ]) `shouldBe` ( [1, 4, 7, 10], [2, 5, 8, 11], [3, 6, 9, 12] )


    describe "mergeSorted3" $ do
        context "mergeSorted3 [2, 3, 5] [1, 8] [-1, 0, 4, 10]" $
            it "should be [-1, 0, 1, 2, 3, 4, 5, 8, 10]" $
                (mergeSorted3 [2, 3, 5] [1, 8] [-1, 0, 4, 10]) `shouldBe` [-1, 0, 1, 2, 3, 4, 5, 8, 10]

        context "mergeSorted3 [2, 3, 5] [1, 7, 8] [4, 6, 9]" $
            it "should be [1, 2, 3, 4, 5, 6, 7, 8, 9]" $
                (mergeSorted3 [2, 3, 5] [1, 7, 8] [4, 6, 9]) `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9]

        context "mergeSorted3 [] [] []" $
            it "should be []" $
                (mergeSorted3 [] [] [] :: [Integer]) `shouldBe` []





        

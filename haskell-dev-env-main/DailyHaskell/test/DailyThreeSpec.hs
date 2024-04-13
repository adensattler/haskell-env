module DailyThreeSpec where

import Test.Hspec
import DailyThree

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "removeAllExcept" $ do
        context "removeAllExcept 'a' ['b', 'a', 'c', 'a']" $
            it "should be \"aa\"" $
                (removeAllExcept 'a' ['b', 'a', 'c', 'a']) `shouldBe` "aa"
        
        context "removeAllExcept 1 [2, 3, 4, 1]" $
            it "should be [1]" $
                (removeAllExcept 1 [2, 3, 4, 1]) `shouldBe` [1]
        
        context "removeAllExcept 1 []" $
            it "should be [1]" $
                (removeAllExcept 1 []) `shouldBe` []
        
    describe "countOccurrences" $ do
        context "countOccurrences 'a' ['a', 'b', 'a', 'c'" $
            it "should be 2" $
                (countOccurrences 'a' ['a', 'b', 'a', 'c']) `shouldBe` 2

        context "countOccurrences 1 [2, 4, 5, 2] " $
            it "should be 0" $
                (countOccurrences 1 [2, 4, 5, 2] ) `shouldBe` 0
        
        context "countOccurrences 1 [] " $
            it "should be 0" $
                (countOccurrences 1 [] ) `shouldBe` 0

    describe "substitute " $ do
        context "substitute 3 4 [1, 2, 3, 4]" $
            it "should be [1, 2, 4, 4]" $
                (substitute 3 4 [1, 2, 3, 4]) `shouldBe` [1, 2, 4, 4]

        context "substitute 3 4 []" $
            it "should be []" $
                (substitute 3 4 []) `shouldBe` []

        context "substitute 'a' 'b' \"abacadabra\"" $
            it "should be \"bbbcbdbbrb\"" $
                (substitute 'a' 'b' "abacadabra") `shouldBe` "bbbcbdbbrb"

module DailyNineSpec where

import Test.Hspec
import DailyNine

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "onlyNothing" $ do
        context "test" $
            it "should be True" $
                onlyNothing (\x -> Nothing) [1, 2, 3] `shouldBe` True

        context "when at least one element produces Just" $
            it "returns False" $
                onlyNothing (\x -> if x `mod` 2 == 0 then Just (x) else Nothing) [1, 2, 3] `shouldBe` False

        context "when the list is empty" $
            it "returns True" $
                onlyNothing (\x -> Nothing) ([] :: [Int]) `shouldBe` True


    describe "firstAnswer" $ do
        context "firstAnswer (Just v if v%2==0 else Nothing) [1, 2, 3, 4, 5]" $
            it "should be 2" $
                firstAnswer (\x -> if x `mod` 2 == 0 then Just (x) else Nothing) [1, 2, 3, 4, 5] `shouldBe` (Just 2 :: Maybe Int)

        context "when all elements produce Nothing" $
            it "returns Nothing" $
                firstAnswer (\x -> Nothing) [1, 2, 3] `shouldBe` (Nothing :: Maybe Int)

        context "when the list is empty" $
            it "returns Nothing" $
                firstAnswer (\x -> Nothing) ([] :: [Int]) `shouldBe` (Nothing :: Maybe Int)

    describe "allAnswers" $ do
        context "when all elements produce Just lists" $
            it "returns Just concatenated list" $
                allAnswers (\x -> if (x `mod` 2) == 0 then Just [x] else Nothing) [2, 4] `shouldBe` (Just [2, 4] :: Maybe [Int])

        context "when at least one element produces Nothing" $
            it "returns Nothing" $
                allAnswers (\x -> if (x `mod` 2) == 0 then Just [x] else Nothing) [1, 2, 3] `shouldBe` (Nothing :: Maybe [Int])

        context "when the list is empty" $
            it "returns Just []" $
                allAnswers (\_ -> Nothing) ([] :: [Int]) `shouldBe` (Just [] :: Maybe [Int])

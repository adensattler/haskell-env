module WeeklyThreeSpec where

import Test.Hspec
import WeeklyThree

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Vec" $ do
        context "shorterThan 3 [\"cat\", \"dog\", \"elephant\"]" $
            it "should be [\"cat\", \"dog\"]" $
                (shorterThan 3 ["cat", "dog", "elephant"]) `shouldBe` ["cat", "dog"]
        
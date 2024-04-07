module DailyOneSpec where

import Test.Hspec
import DailyOne

spec :: Spec
spec = do
    describe "quadratic" $ do
        context "when x is zero" $
            it "produces the correct quadratic" $
                quadratic 0 0 0 1 `shouldBe` 0.0

        context "when coefficients are non-zero" $
            it "produces the correct quadratic" $
                quadratic 4 3 2 0 `shouldBe` 4.0

    describe "scaleVector" $ do
        context "when scaling by 0" $
            it "produces (0, 0)" $
                scaleVector 0 (3, 4) `shouldBe` (0.0, 0.0)

        context "when scaling by 2" $
            it "produces the scaled vector" $
                scaleVector 2 (3, 4) `shouldBe` (6.0, 8.0)

    describe "tripleDistance" $ do
        context "when points are the same" $
            it "produces 0" $
                tripleDistance (1, 1, 1) (1, 1, 1) `shouldBe` 0.0

        context "when points are different" $
            it "produces the correct distance" $
                tripleDistance (0, 0, 0) (3, 4, 12) `shouldBe` 13.0

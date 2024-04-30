module WeeklyThreeSpec where

import Test.Hspec
import WeeklyThree

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Vec" $ do
        let vec1 = Vec [1.0, 2.0, 3.0]
            vec2 = Vec [4.0, 5.0, 6.0]

        context "Show instance" $ do
            it "should show properly" $ do
                vec1 `shouldBe` Vec [1.0, 2.0, 3.0]

        context "Num instance" $ do
            it "should add vectors element-wise" $ do
                vec1 + vec2 `shouldBe` Vec [5.0, 7.0, 9.0]
            it "should subtract vectors element-wise" $ do
                vec1 - vec2 `shouldBe` Vec [-3.0, -3.0, -3.0]
            it "should multiply vectors element-wise" $ do
                vec1 * vec2 `shouldBe` Vec [4.0, 10.0, 18.0]
            it "should negate the vector" $ do
                negate vec1 `shouldBe` Vec [-1.0, -2.0, -3.0]
            it "should return absolute values of elements" $ do
                abs (Vec [-1.0, 2.0, -3.0]) `shouldBe` Vec [1.0, 2.0, 3.0]
            it "should return the signum of the vector" $ do
                signum (Vec [-1.0, 2.0, -3.0]) `shouldBe` Vec [-1.0, 1.0, -1.0]
            it "should convert from Integer" $ do
                fromInteger 3 `shouldBe` Vec [3.0, 3.0, 3.0, 3.0]

        context "Eq instance" $ do
            it "should check equality of vectors" $ do
                vec1 == vec2 `shouldBe` False
                vec1 == (Vec [1.0, 2.0, 3.0]) `shouldBe` True

        context "Ord instance" $ do
            it "should compare vectors lexicographically" $ do
                compare vec1 vec2 `shouldBe` LT
                compare vec2 vec1 `shouldBe` GT
                compare vec1 vec1 `shouldBe` EQ
            it "should dertermine min/max vectors lexicographically" $ do
                min vec1 vec2 `shouldBe` vec1
                max vec1 vec2 `shouldBe` vec2

        context "VecT instance" $ do
            it "should calculate the magnitude of a vector" $ do
                magnitude vec1 `shouldBe` sqrt 14.0

        context "Semigroup instance" $ do
            it "should combine vectors using addition" $ do
                vec1 <> vec2 `shouldBe` Vec [5.0, 7.0, 9.0]

        context "Monoid instance" $ do
            it "should return the identity value" $ do
                mempty `shouldBe` Vec [0.0, 0.0, 0.0, 0.0]
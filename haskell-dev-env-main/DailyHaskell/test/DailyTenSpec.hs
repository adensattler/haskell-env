module DailyTenSpec where

import Test.Hspec
import DailyTen

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "allLefts" $ do
        context "allLefts [(Left \"hi\"), (Left \"test\"), (Left \"bye\")]" $
            it "should be " $
                (allLefts [(Left "hi"), (Left "test"), (Left "bye")]) `shouldBe` ["hi", "test", "bye"]

        context "allLefts [(Left \"a\"), (Right 4), (Left \"b\")]" $
            it "should be [\"a\", \"b\"]" $
                (allLefts [(Left "a"), (Right 4), (Left "b")]) `shouldBe` ["a", "b"]
        
        context "allLefts [(Right 4), (Right 5)]" $
            it "should be []" $
                (allLefts [(Right 4), (Right 5)]) `shouldBe` ([] :: [Integer])
        
        context "allLefts []" $
            it "should be []" $
                (allLefts []) `shouldBe` ([] :: [String])


    describe "produceStringOrSum" $ do
        context "produceStringOrSum (Left \"hi\") (Right 3)" $
            it "should be Left \"hi\"" $
                (produceStringOrSum (Left "hi") (Right 3)) `shouldBe`(Left "hi")
        
        context "produceStringOrSum (Left \"hi\") (Left \"bye\")" $
            it "should be Left \"hi\"" $
                (produceStringOrSum (Left "hi") (Left "bye")) `shouldBe`(Left "hi")
        
        context "produceStringOrSum (Right 3) (Left \"bye\")" $
            it "should be Left \"bye\"" $
                (produceStringOrSum (Right 3) (Left "bye")) `shouldBe`(Left "bye")
        
        context "produceStringOrSum (Right 4) (Right 3)" $
            it "should be Right 7" $
                (produceStringOrSum (Right 4) (Right 3)) `shouldBe` (Right 7)


    describe "sumListOfEither" $ do
        context "sumListOfEither [(Right 1), (Left \"test\"), (Right 5)]" $
            it "should be Left \"test\"" $
                (sumListOfEither [(Right 1), (Left "test"), (Right 5)]) `shouldBe` (Left "test")
        
        context "sumListOfEither [(Right 1), (Right 5)]" $
            it "should be Right 6" $
                (sumListOfEither [(Right 1), (Right 5)]) `shouldBe` (Right 6)
        
        context "sumListOfEither []" $
            it "should be 0" $
                (sumListOfEither []) `shouldBe` (Right 0)
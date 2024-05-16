module DailyElevenSpec where

import Test.Hspec
import DailyEleven
import Data.Char

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "firstFunctorLaw" $ do
        context "firstFunctorLaw (Just ('c', 35))" $
            it "should be True" $
                (firstFunctorLaw (Just ('c', 35))) `shouldBe` True
        context "firstFunctorLaw [2, 3, 5, 7, 11]" $
            it "should be True" $
                (firstFunctorLaw [2, 3, 5, 7, 11]) `shouldBe` True


    describe "secondFunctorLaw" $ do
        context "secondFunctorLaw isAlpha fst (Just ('c', 35))" $
            it "should be True" $
                (secondFunctorLaw isAlpha fst (Just ('c', 35))) `shouldBe` True
        context "secondFunctorLaw chr (+96) [2, 3, 5, 7, 11]" $
            it "should be True" $
                (secondFunctorLaw chr (+96) [2, 3, 5, 7, 11]) `shouldBe` True

    -- Either String (Maybe Integer) Follows Law 1
    describe "testing firstFunctorLaw with (Either String (Maybe Integer))" $ do
        -- Test Left
        context "firstFunctorLaw (Left \"some error\") :: Either String (Maybe Integer)" $
            it "should be True" $
                (firstFunctorLaw ((Left "some error") :: Either String (Maybe Integer))) `shouldBe` True
        -- Test Right Nothing
        context "firstFunctorLaw (Right Nothing) :: Either String (Maybe Integer)" $
            it "should be True" $
                (firstFunctorLaw ((Right Nothing) :: Either String (Maybe Integer))) `shouldBe` True
        -- Test Right (Just k)
        context "firstFunctorLaw (Right (Just 13)) :: Either String (Maybe Integer)" $
            it "should be True" $
                (firstFunctorLaw ((Right (Just 13)) :: Either String (Maybe Integer))) `shouldBe` True

    -- Either String (Maybe Integer) Follows Law 2
    describe "testing secondFunctorLaw with (Either String (Maybe Integer))" $ do
        -- Test Left
        context "secondFunctorLaw incInteger dubInteger ((Left \"some error\") :: Either String (Maybe Integer)" $
            it "should be True" $
                (secondFunctorLaw incInteger dubInteger ((Left "some error") :: Either String (Maybe Integer))) `shouldBe` True
        -- Test Right Nothing
        context "secondFunctorLaw incInteger dubInteger ((Right Nothing) :: Either String (Maybe Integer)" $
            it "should be True" $
                (secondFunctorLaw incInteger dubInteger ((Right Nothing) :: Either String (Maybe Integer))) `shouldBe` True
        -- Test Right (Just 13)
        context "secondFunctorLaw incInteger dubInteger ((Right (Just 13)) :: Either String (Maybe Integer)" $
            it "should be True" $
                (secondFunctorLaw incInteger dubInteger ((Right (Just 13)) :: Either String (Maybe Integer))) `shouldBe` True
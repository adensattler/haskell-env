module DailySevenSpec where

import Test.Hspec
import DailySeven

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "findLongest" $ do
        context "findLongest [\"words\", \"for\", \"testing\"]" $
            it "should be \"testing\"" $
                (findLongest ["words", "for", "testing"]) `shouldBe` "testing"
        context "findLongest [\"words\", \"for\", \"testing\", \"mesting\"]" $
            it "should be \"testing\"" $
                (findLongest ["words", "for", "testing", "mesting"]) `shouldBe` "testing"
        context "findLongest []" $
            it "should be " $
                (findLongest []) `shouldBe` ""      
    
    describe "anyLarger" $ do
        context "anyLarger 5 [1,2,3,4]" $
            it "should be False" $
                (anyLarger 5 [1,2,3,4]) `shouldBe` False
        context "anyLarger 5 [1,3,5,2]" $
            it "should be True" $
                (anyLarger 5 [1,3,5,2]) `shouldBe` True
        context "anyLarger 2 []" $
            it "should be False" $
                (anyLarger 2 []) `shouldBe` False


    describe "allNames" $ do
        context "allNames [(\"kermit\", \"the frog\"), (\"bugs\", \"bunny\")]" $
            it "should be \"kermit the frog, bugs bunny\"" $
                (allNames [("kermit", "the frog"), ("bugs", "bunny")]) `shouldBe` "kermit the frog, bugs bunny"
        context "allNames [(\"kermit\", \"the frog\")]" $
            it "should be \"kermit the frog, bugs bunny\"" $
                (allNames [("kermit", "the frog")]) `shouldBe` "kermit the frog"
        context "allNames []" $
            it "should be \"\"" $
                (allNames []) `shouldBe` ""
        
    
        
        
        
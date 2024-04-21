module TriTreeSpec where

import Test.Hspec
import TriTree

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "search" $ do
        -- search empty tree
        context "search 1 Empty" $ do
            it "should be False" $
                search 1 Empty `shouldBe` False

        -- val in leaf
        context "search 1 (Leaf 1)" $ do
            it "should be True" $
                search 1 (Leaf 1) `shouldBe` True
        
        -- val in Leaf within Node
        context "search 1 (Node 3 5 (Leaf 1) Empty Empty)" $ do
            it "should be True" $
                search 1 (Node 3 5 (Leaf 1) Empty Empty) `shouldBe` True
        
        -- val in v1
        context "search 1 (Node 3 5 (Leaf 1) Empty Empty)" $ do
            it "should be True" $
                search 1 (Node 3 5 (Node 1 2 Empty Empty Empty) Empty Empty) `shouldBe` True

        -- val in v2
        context "search 2 (Node 3 5 (Leaf 1) Empty Empty)" $ do
            it "should be True" $
                search 2 (Node 3 5 (Node 1 2 Empty Empty Empty) Empty Empty) `shouldBe` True

        -- val not in leftTree
        context "search 0 (Node 3 5 (Leaf 1) Empty Empty)" $ do
            it "should be True" $
                search 0 (Node 3 5 (Node 1 2 Empty Empty Empty) Empty Empty) `shouldBe` False

        -- val in mid then right subtree
        context "search 17 (Node 3 5 (Leaf 1) Empty Empty)" $ do
            it "should be True" $
                search 17 (Node 10 20 Empty (Node 13 16 Empty Empty (Node 17 19 Empty Empty Empty)) Empty) `shouldBe` True



    describe "insert" $ do
        context "insert 1 (Leaf 2)" $
            it "should be Node 1 2 Empty Empty Empty" $
                (insert 1 (Leaf 2)) `shouldBe` Node 1 2 Empty Empty Empty

        context "insert 2 (Leaf 1)" $
            it "should be Node 1 2 Empty Empty Empty" $
                (insert 2 (Leaf 1)) `shouldBe` Node 1 2 Empty Empty Empty

        context "insert 3 (Node 1 2 Empty Empty Empty)" $
            it "should be Node 1 2 Empty Empty (Leaf 3)" $
                (insert 3 (Node 1 2 Empty Empty Empty) ) `shouldBe` Node 1 2 Empty Empty (Leaf 3)

        context "insert 2 (Node 1 3 Empty Empty Empty)" $
            it "should be Node 1 3 Empty (Leaf 2) Empty" $
                (insert 2 (Node 1 3 Empty Empty Empty) ) `shouldBe` Node 1 3 Empty (Leaf 2) Empty

        context "insert 1 (Node 2 3 Empty Empty Empty)" $
            it "should be Node 2 3 (Leaf 1) Empty Empty" $
                (insert 1 (Node 2 3 Empty Empty Empty)) `shouldBe` Node 2 3 (Leaf 1) Empty Empty
    
    

    describe "insertList" $ do
        context "insertList [] Empty" $
            it "should be an empty tree" $
                (insertList [] Empty) `shouldBe` (Empty :: TriTree Int)

        context "insertList [1..5] Empty" $
            it "should be Node 4 5 (Node 2 3 (Leaf 1) Empty Empty) Empty Empty" $
                (insertList [1..5] Empty) `shouldBe` Node 4 5 (Node 2 3 (Leaf 1) Empty Empty) Empty Empty

        context "insertList [5, 4, 3, 2, 1] Empty" $
            it "should be Node 1 2 Empty Empty (Node 3 4 Empty Empty (Leaf 5))" $
                (insertList [5, 4, 3, 2, 1] Empty) `shouldBe` (Node 1 2 Empty Empty (Node 3 4 Empty Empty (Leaf 5)))



    describe "identical" $ do
        context "identical Empty Empty" $
            it "should be True" $
                (identical (Empty :: TriTree Int) (Empty :: TriTree Int)) `shouldBe` True

        context "identical (Leaf 1) (Leaf 1)" $
            it "should be True" $
                identical (Leaf 1) (Leaf 1) `shouldBe` True

        context "identical (Node 1 2 (Leaf 1) Empty (Leaf 2)) (Node 1 2 (Leaf 1) Empty (Leaf 3))" $
            it "should be False for two different trees" $
                identical (Node 1 2 (Leaf 1) Empty (Leaf 2)) (Node 1 2 (Leaf 1) Empty (Leaf 3)) `shouldBe` False

        context "identical (Node 1 2 (Leaf 1) Empty (Leaf 2)) (Node 1 2 (Leaf 1) Empty (Leaf 2))" $
            it "should be True" $
                identical (Node 1 2 (Leaf 1) Empty (Leaf 2)) (Node 1 2 (Leaf 1) Empty (Leaf 2)) `shouldBe` True



    describe "treeMap" $ do
        context "treeMap (*2) Empty" $
            it "should be an empty tree" $
                treeMap (*2) Empty `shouldBe` (Empty :: TriTree Int)

        context "treeMap (*2) (Leaf 3)" $
            it "should be a leaf with value doubled" $
                treeMap (*2) (Leaf 3) `shouldBe` Leaf 6

        context "treeMap (*2) (Node 1 2 (Leaf 1) Empty (Leaf 2))" $
            it "should double each value in the tree" $
                treeMap (*2) (Node 1 2 (Leaf 1) Empty (Leaf 2)) `shouldBe` Node 2 4 (Leaf 2) Empty (Leaf 4)
    


    describe "treeFoldPreOrder" $ do
        context "treeFoldPreOrder (+) 0 Empty" $
            it "should return the initial accumulator for an empty tree" $
                treeFoldPreOrder (+) 0 Empty `shouldBe` 0

        context "treeFoldPreOrder (+) 0 (Leaf 1)" $
            it "should return the value of the leaf plus the initial accumulator" $
                treeFoldPreOrder (+) 0 (Leaf 1) `shouldBe` 1

        context "treeFoldPreOrder (+) 0 (Node 1 2 (Leaf 1) Empty (Leaf 2))" $
            it "should sum all values in the tree" $
                treeFoldPreOrder (+) 0 (Node 1 2 (Leaf 1) Empty (Leaf 2)) `shouldBe` 6

        context "treeFoldPreOrder (++) \"\" (Node \"a\" \"b\" (Leaf \"c\") Empty (Leaf \"d\"))" $
            it "should concatenate all values in the tree" $
                treeFoldPreOrder (++) "" (Node "a" "b" (Leaf "c") Empty (Leaf "d")) `shouldBe` "abcd"
    
    
    
    describe "treeFoldInOrder" $ do
        context "treeFoldInOrder (++) \"\" (Leaf \"a\")" $
            it "should concatenate the value of the leaf with the initial accumulator" $
                treeFoldInOrder (++) "" (Leaf "a") `shouldBe` "a"

        context "treeFoldInOrder (++) \"\" (Node \"a\" \"b\" Empty (Leaf \"c\") Empty)" $
            it "should concatenate all values in the tree in order" $
                treeFoldInOrder (++) "" (Node "a" "b" Empty (Leaf "c") Empty) `shouldBe` "acb"

        context "treeFoldInOrder (++) \"\" (Node \"a\" \"b\" (Leaf \"c\") Empty Empty)" $
            it "should concatenate all values in the tree in order" $
                treeFoldInOrder (++) "" (Node "a" "b" (Leaf "c") Empty Empty) `shouldBe` "cab"



    describe "treeFoldPostOrder" $ do
        context "treeFoldPostOrder (+) 0 Empty" $
            it "should return the initial accumulator for an empty tree" $
                treeFoldPostOrder (+) 0 Empty `shouldBe` 0

        context "treeFoldPostOrder (+) 0 (Leaf 1)" $
            it "should return the value of the leaf plus the initial accumulator" $
                treeFoldPostOrder (+) 0 (Leaf 1) `shouldBe` 1

        context "treeFoldPostOrder (+) 0 (Node 1 2 (Leaf 1) Empty (Leaf 2))" $
            it "should sum all values in the tree" $
                treeFoldPostOrder (+) 0 (Node 1 2 (Leaf 1) Empty (Leaf 2)) `shouldBe` 6

        context "treeFoldPostOrder (++) \"\" (Leaf \"a\")" $
            it "should concatenate the value of the leaf with the initial accumulator" $
                treeFoldPostOrder (++) "" (Leaf "a") `shouldBe` "a"

        context "treeFoldPostOrder (++) \"\" (Node \"a\" \"b\" (Leaf \"c\") Empty Empty)" $
            it "should concatenate all values in the tree in post-order" $
                treeFoldPostOrder (++) "" (Node "a" "b" (Leaf "c") Empty Empty) `shouldBe` "cab"
    




    -- describe "temp" $ do
    --     context "temp" $
    --         it "should be " $
    --             (temp) `shouldBe` 
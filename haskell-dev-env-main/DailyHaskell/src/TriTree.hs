module TriTree where

    data TriTree a = Empty | 
                     Leaf a | 
                     Node a a (TriTree a) (TriTree a) (TriTree a) 
                     deriving (Eq,Show)

    -- Searches for a value in the tree.
    -- Parameters:
    --   - value: the value to search for
    --   - tree: the tree to search in
    -- Result:
    --   - True if the value is found in the tree, False otherwise.
    search :: (Ord a) => a -> TriTree a -> Bool
    search _ Empty = False
    search value (Leaf v) = (v==value)
    search value (Node v1 v2 leftTree midTree rightTree) =
        if value == v1 || value == v2
            then True
        else if value <= v1
            then search value leftTree
        else if value <= v2
            then search value midTree
        else
            search value rightTree


    -- Inserts a value into the tree.
    -- Parameters:
    --   - value: the value to insert
    --   - tree: the tree to insert into
    -- Result:
    --   - The modified tree with the value inserted.
    insert :: (Ord a) => a -> TriTree a -> TriTree a
    insert value Empty = Leaf value
    insert value (Leaf v) = 
        if value <= v 
            then Node value v Empty Empty Empty
        else Node v value Empty Empty Empty
    insert value (Node v1 v2 leftTree midTree rightTree) =
        if value <= v1
            then Node v1 v2 (insert value leftTree) midTree rightTree
        else if value <= v2
            then Node v1 v2 leftTree (insert value midTree) rightTree
        else
            Node v1 v2 leftTree midTree (insert value rightTree)


    -- Inserts a list of values into the tree.
    -- Parameters:
    --   - vals: the list of values to insert
    --   - tree: the tree to insert into
    -- Result:
    --   - The modified tree with the values inserted.
    insertList :: (Ord a) => [a] -> TriTree a -> TriTree a
    insertList vals tree = foldr (insert) tree vals


    -- Checks if two trees are identical.
    -- Parameters:
    --   - tree1: the first tree
    --   - tree2: the second tree
    -- Result:
    --   - True if the trees are identical, False otherwise.
    identical :: (Ord a) => TriTree a -> TriTree a -> Bool
    identical Empty Empty = True
    identical (Leaf x) (Leaf y) = x==y
    identical (Node v1 v2 left1 mid1 right1) (Node v3 v4 left2 mid2 right2) = 
        if v1 == v3 && v2 == v4 
            then identical left1 left2 && identical mid1 mid2 && identical right1 right2
        else False
    identical _ _ = False


    -- Maps a function over the values of the tree.
    -- Parameters:
    --   - f: the mapping function
    --   - tree: the tree to map over
    -- Result:
    --   - The tree with the function applied to each value.
    treeMap :: (a -> b) -> TriTree a -> TriTree b
    treeMap _ Empty = Empty
    treeMap f (Leaf x) = Leaf (f x)
    treeMap f (Node v1 v2 left mid right) = Node (f v1) (f v2) (treeMap f left) (treeMap f mid) (treeMap f right)
    
    
    -- Folds over the tree in pre-order traversal.
    -- Parameters:
    --   - f: the folding function
    --   - acc: the initial accumulator value
    --   - tree: the tree to fold over
    -- Result:
    --   - The result of folding over the tree.
    treeFoldPreOrder :: (a -> b -> a) -> a -> TriTree b -> a
    -- Base case to return the accumulator at an empty node
    treeFoldPreOrder _ acc Empty = acc
    treeFoldPreOrder f acc (Leaf x) = f acc x
    treeFoldPreOrder f acc (Node v1 v2 left mid right) = 
        let 
            acc2 = f acc v1
            acc3 = f acc2 v2
            acc4 = treeFoldPreOrder f acc3 left
            acc5 = treeFoldPreOrder f acc4 mid
        in treeFoldPreOrder f acc5 right
    

    -- Folds over the tree in in-order traversal.
    -- Parameters:
    --   - f: the folding function
    --   - acc: the initial accumulator value
    --   - tree: the tree to fold over
    -- Result:
    --   - The result of folding over the tree.
    treeFoldInOrder :: (a -> b -> a) -> a -> TriTree b -> a
    treeFoldInOrder _ acc Empty = acc
    treeFoldInOrder f acc (Leaf x) = f acc x
    treeFoldInOrder f acc (Node v1 v2 left mid right) = 
        let 
            acc2 = treeFoldInOrder f acc left
            acc3 = f acc2 v1
            acc4 = treeFoldInOrder f acc3 mid
            acc5 = f acc4 v2
        in treeFoldInOrder f acc5 right


    -- Folds over the tree in post-order traversal.
    -- Parameters:
    --   - f: the folding function
    --   - acc: the initial accumulator value
    --   - tree: the tree to fold over
    -- Result:
    --   - The result of folding over the tree.
    treeFoldPostOrder :: (a -> b -> a) -> a -> TriTree b -> a
    treeFoldPostOrder _ acc Empty = acc  -- Base case: return the accumulator for an empty tree
    treeFoldPostOrder f acc (Leaf x) = f acc x  
    treeFoldPostOrder f acc (Node v1 v2 left mid right) =
        let acc1 = treeFoldPostOrder f acc left  
            acc2 = treeFoldPostOrder f acc1 mid    
            acc3 = treeFoldPostOrder f acc2 right  
            acc4 = f acc3 v1                      
        in f acc4 v2            

    
    
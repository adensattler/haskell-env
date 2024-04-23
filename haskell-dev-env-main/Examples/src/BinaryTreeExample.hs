module BinaryTreeExample where
    import Data.Char(isSpace)
    
data BinTree a = EmptyTree
                | Leaf a 
                | Node a (BinTree a) (BinTree a)
                deriving( Show )


-- Insert a Value

--  Consume a BinTree and a value 
--  Produce a BinTree with the value inserted
-- => think java interface. this has to live to to some stuff
insert :: (Ord a) => a -> BinTree a -> BinTree a
insert value EmptyTree = Leaf value 
insert value (Leaf v) = if value <= v 
                            then Node v (Leaf value) EmptyTree 
                            else Node value EmptyTree (Leaf v)
insert value (Node v leftTree rightTree) = if value <= value
                                                then (Node v (insert value leftTree) rightTree)
                                                else (Node v leftTree (insert value rightTree))

search :: (Ord a ) => a -> BinTree a -> Bool
search value EmptyTree = False 
search value (Leaf v) = value  == v
search value (Node v leftTree rightTree) = if value == v
        then True 
        else if value < v 
            then search value leftTree
            else search value rightTree

treeMap :: (a->b) -> BinTree a -> BinTree b
treeMap f EmptyTree = EmptyTree
treeMap f (Leaf value) = (Leaf (f value))
treeMap f (Node value leftChild rightChild) = (Node (f value) (treeMap f leftChild) (treeMap f rightChild))


-- foldl :: (a -> b -> a) -> a -> [a] -> a



instance Functor BinTree where 
    fmap _ EmptyTree = EmptyTree
    fmap f (Leaf value) = (Leaf (f value))
    fmap f (Node value leftChild rightChild) = Node (f value) (fmap f leftChild) (fmap f rightChild)


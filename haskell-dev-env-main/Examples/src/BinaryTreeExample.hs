module BinaryTreeExample where

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


data Token = BoolValueToken Bool          
                | AndToken
                | OrToken
                | NotToken
                | LeftParenToken
                | RightParenToken
                deriving (Show)
lexString :: String => [Token]
lexString [] = []
lexString ('T':'r':'u':'e':remain) = BoolValueToken True : lexString remain
lexString ('F':'a':'l':'s':'e':remain) = BoolValueToken False : lexString remain
lexString ('&':'&':remain) = AndToken : lexString remain
lexString ('|':'|':remain) = OrToken : lexString remain
lexString ('!':remain) = NotToken : lexString remain
lexString ('(':remain) = NotToken : lexString remain
lexString (')':remain) = NotToken : lexString remain
lexString (' ':remain) = lexString remain
lexString _ = error "Syntax Error: Unrecognized Symbol"
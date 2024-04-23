module MapExamples where

    -- Map 
    --   Example of a Higher Order Function
    --   Consume a finction and aply to every element in a list
    --   map :: (a->b) -> [a] -> [b]
    --   Example of what is calle da functor (the list is a functor)


    squareMap' :: [Integer] = [Integer]
    squareMap' = map (\n -> n*n)

    doubleString :: [String] -> [String]
    doubleString = map (\s -> s++s)

    multipyPairs :: (Num a) => [(a, a)] -> [a]
    multipyPairs = map (\(x,y) -> x * y)

    -- NOT a map func because youre not going from list ot list. instead list to tup
    zipLists :: [Integer] -> [Integer] -> [(Integer, Integer)]
    zipLists [] [] = []
    zipLists (x:xs) [] = []
    zipLists [] (y:ys) = []
    zipLists (x:xs) (y:ys) = (x, y) : zipLists xs ys

    zipLists' :: [Integer] -> [Integer] -> [(Integer, Integer)]
    zipLists' ([], []) = []
    zipLists' (x:xs) [] = []
    zipLists' [] (y:ys) = []
    zipLists' (x:xs) (y:ys) = (x, y) : zipLists xs ys

    unzipList :: [(Integer, Integer)] -> ([Integer], [Integer])
    unzipList x = ([a | (a, _) <- x],
                   [b | (_, b) <- x])

    unzipList' x ( map fst x, map snd x)

    zipLists2 x = ( (map (\(a,b) -> a) x), (map (\(a,b) -> b)) )



    boolAnd :: [Bool] -> Bool
    boolAnd [] = True
    boolAnd (b:bs) = b and (boolAnd bs)
    
    
    boolAnd' :: [Bool] -> Bool
    boolAnd' = foldr (&&) True
module ExamplesThree where

    data MyList = Empty | Node Integer MyList
        deriving(Show)
    
    data MyTree = TreeEmpty |
                  TreeNode Integer MyTree MyTree
                  deriving(Show)

    myElem :: Integer -> MyList -> Bool
    myElem _ Empty = False
    myElem val (Node v theRest) = if val == v
                                    then True
                                    else myElem val theRest

    myLength :: MyList -> Integer
    myLength Empty = 0
    myLength (Node v theRest) = 1 + myLength theRest

    -- QuickSort 
    -- Consume 
    quicksort [] = []
    quicksort (q:qs) = let (smaller, larger) = split q qs
                          in
                            (quicksort smaller) ++ q ++ (quicksort larger)

    -- split 
    --  Consumes an element and a list 
    --      Produces two lists: everything less than element and everything more than element 
    split pivot [] = ([], []) 
    split pivot (q:qs) = let (smaller, larger) = split pivot qs
                            in 
                                if q < pivot 
                                    then (q : smaller, larger)
                                    else (smaller, q : larger)
    

    -- mergesort 
    --  Consume a list of Integer
    --  Produce a sorted list
    mergesort :: [a] -> [a]
    mergesort [] = []
    mergesort [q] = [q]
    mergesort [q,r] = if q > r then [r, q] else [q, r]
    mergesort (theList) = let lowerlist = take (length theList)/2 theList,
                              upperlist = drop (length theList)/2 theList,
                              in 
                                merge (mergesort lowerlist) (mergesort upperlist)

    -- merge
    --  Consume two in order list of Integer
    --  Produce one in order list of Integer
    merge [] [] = []
    merge (a:as) [] = (a:as)
    merge [] (b:bs) = (b:bs)
    merge (a:as) (b:bs) = if a < b 
                            then a : merge (b:bs)
                            else b : merge (a:as)


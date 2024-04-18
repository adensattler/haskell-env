module DailyFour where

    -- Constructs a list of tuples from three input lists, each tuple consisting of
    -- elements from the corresponding positions in the input lists
    -- Parameters:
    --   - list1: the first input list
    --   - list2: the second input list
    --   - list3: the third input list
    -- Result:
    --   - List of tuples where each tuple contains elements from list1, list2, and list3 consecutively
    zip3Lists :: [a] -> [b] -> [c] -> [(a, b, c)]
    zip3Lists _ _ [] = []
    zip3Lists _ [] _ = []
    zip3Lists [] _ _ = []
    zip3Lists (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3Lists xs ys zs
    
    -- Unzips a list of triples into three separate lists, each containing elements from
    -- the corresponding position in the input triples
    -- Parameters:
    --   - triples: the input list of triples
    -- Result:
    --   - Tuple of three lists where each list contains elements from the input triples
    unzipTriples :: [(a, b, c)] -> ([a], [b], [c])
    unzipTriples [] = ([], [], [])
    unzipTriples ((x, y, z):remaining) = 
        let (xs, ys, zs) = unzipTriples remaining
            in (x:xs, y:ys, z:zs)


    -- Merges three sorted lists into one sorted list in increasing order
    -- Parameters:
    --   - list1: the first sorted list
    --   - list2: the second sorted list
    --   - list3: the third sorted list
    -- Result:
    --   - Merged list containing all elements from list1, list2, and list3 in increasing order
    mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
    mergeSorted3 [] ys zs = mergeSorted ys zs
    mergeSorted3 xs [] zs = mergeSorted xs zs
    mergeSorted3 xs ys [] = mergeSorted xs ys
    mergeSorted3 (x:xs) (y:ys) (z:zs) = 
        if x <= y && x <= z then x:mergeSorted3 xs (y:ys) (z:zs)
        else if y <= x && y <= z then y:mergeSorted3 (x:xs) ys (z:zs)
        else z:mergeSorted3 (x:xs) (y:ys) zs


    -- Merges two sorted lists into one sorted list in increasing order
    -- Parameters:
    --   - xs: the first sorted list
    --   - ys: the second sorted list
    -- Result:
    --   - Merged list containing all elements from xs and ys in increasing order
    mergeSorted :: Ord a => [a] -> [a] -> [a]
    mergeSorted xs [] = xs
    mergeSorted [] ys = ys
    mergeSorted (x:xs) (y:ys) = 
        if x <= y then x:mergeSorted xs (y:ys)
        else y:mergeSorted (x:xs) ys

module DailyThree where

    -- Removes all elements from a list except the ones equal to the given target
    -- Parameters:
    --   - target: the element to keep in the list
    --   - list: the input list of any type 'a'
    -- Result:
    --   - List containing only elements equal to the target
    removeAllExcept :: Eq a => a -> [a] -> [a]
    removeAllExcept _ [] = []
    removeAllExcept target (x:xs) = if x==target then x:removeAllExcept target xs else removeAllExcept target xs

    -- Counts the occurrences of a given target element in a list
    -- Parameters:
    --   - target: the element to count occurrences of
    --   - list: the input list of any type 'a'
    -- Result:
    --   - Number of occurrences of the target element in the list
    countOccurrences :: Eq a => a -> [a] -> Int
    countOccurrences _ [] = 0
    countOccurrences target (x:xs) = if x==target then 1 + countOccurrences target xs else 0 + countOccurrences target xs

    -- Replaces all occurrences of a target element with a substitute element in a list
    -- Parameters:
    --   - target: the element to be replaced
    --   - sub: the element to replace the target with
    --   - list: the input list of any type 'a'
    -- Result:
    --   - List with all occurrences of the target element replaced by the substitute element
    substitute :: Eq a => a -> a -> [a] -> [a]
    substitute _ _ [] = []
    substitute target sub (x:xs) = if x==target then sub:substitute target sub xs else x:substitute target sub xs


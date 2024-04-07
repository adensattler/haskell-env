module DailyTwo where

    -- Extracts every 4th element from a list
    -- Parameters:
    --   - Input list of any type 'a'
    -- Result:
    --   - List containing every 4th element from the input list
    every4th :: [a] -> [a]
    every4th [] = []    -- Base case: empty list
    every4th (_:_:_:x:xs) = x : every4th xs     -- Include every 4th element
    every4th (_:xs) = every4th xs   -- Skip every other element (occurs when a list is shorter than 4 elements)

    -- Calculates the dot product of two lists
    -- Parameters:
    --   - Two lists of numerical type 'a'
    -- Result:
    --   - Dot product of the two input lists
    --     (sum of the products of corresponding elements)
    -- Class constraint: Numeric type for list elements
    tupleDotProduct :: Num a => [a] -> [a] -> a
    tupleDotProduct [] [] = 0   -- Base case: empty lists
    tupleDotProduct (q : qs) (p : ps) = (q*p) + tupleDotProduct qs ps   -- Calculate dot product recursively
    tupleDotProduct _ _ = error "Lists must be of equal length"     -- Error if lists have different lengths


    -- Appends a string to each string in a list
    -- Parameters:
    --   - String to append
    --   - List of strings
    -- Result:
    --   - New list where the string is appended to each string
    appendToEach :: String -> [String] -> [String]
    appendToEach _ [] = []      -- Base case: empty list
    appendToEach str (x : xs) = (x ++ str) : appendToEach str xs    -- Append string recursively
                                -- "++"" is for list concatenation!

    -- Converts a list to a set representation, removing duplicates
    -- Parameters:
    --   - Input list of any type 'a'
    -- Result:
    --   - New list with duplicate elements removed
    toSetList :: Eq a => [a] -> [a]
    toSetList [] = []  -- Base case: empty list
    toSetList (x : xs) = 
        if contains x xs  -- If another x exists in the remaining list
            then toSetList xs  -- Skip x
            else x : toSetList xs  -- Include x in the result
                                
    -- Checks if an element is present in a list. Used as a helper function for toSetList
    -- Parameters:
    --   - Element to search for
    --   - List to search in
    -- Result:
    --   - True if the element is found, False otherwise
    contains :: Eq a => a -> [a] -> Bool
    contains _ [] = False  -- Base case: looking for any element in an empty list
    contains x (y : ys) = 
        if x == y  -- If x matches the head of the list
            then True  -- Found x!
            else contains x ys  -- Continue searching in the remainder


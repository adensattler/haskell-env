module DailyEight where

    -- Finds the smallest element in a list. If the list is empty, returns 'Nothing'.
    -- Parameters:
    --   - xs: the list of elements
    -- Result:
    --   - 'Just' the smallest element in the list, or 'Nothing' if the list is empty.
    findSmallest :: (Ord a) => [a] -> Maybe a
    findSmallest [] = Nothing
    findSmallest xs = Just (minimum xs)
    -- findSmallest (x:xs)= Just (foldl (\smallest cur -> if cur < smallest then cur else smallest) x xs)
    -- ^^^ THIS IS AN ALTERNATE VERSION I MADE BECAUSE IM NOT SURE IF WE ARE ALLOWED TO USE THE minimum function


    -- Checks if all elements in a list are 'True'. If the list is empty, returns 'Nothing'.
    -- Parameters:
    --   - xs: the list of Boolean values
    -- Result:
    --   - 'Just' 'True' if all elements in the list are 'True', 'Just' 'False' otherwise, or 'Nothing' if the list is empty.
    allTrue :: [Bool] -> Maybe Bool
    allTrue [] = Nothing
    allTrue xs = Just (foldl (&&) True xs)


    -- Counts the votes in a list of 'Maybe Bool' values. A 'True' value is a vote in favor
    -- False is a vote against, and Nothing is an undecided vote
    -- Parameters:
    --   - xs: the list of 'Maybe Bool' values representing votes
    -- Result:
    --   - A tuple containing the counts of representatives who voted in favor, against, and who have not yet voted.
    countAllVotes :: [Maybe Bool] -> (Integer, Integer, Integer)
    countAllVotes xs = foldl (\(y, n, u) cur -> if cur == (Just True) then (y+1, n, u) else if cur == (Just False) then (y, n+1, u) else (y, n, u+1)) (0, 0, 0) xs
    -- countAllVotes xs = foldl countVotes (0, 0, 0) xs
    --     where 
    --         countVotes (y, n, u) (Just True) = (y+1, n, u)
    --         countVotes (y, n, u) (Just False) = (y, n+1, u)
    --         countVotes (y, n, u) Nothing = (y, n, u+1)
    -- ^^^ THIS IS AN ALTERNATE VERSION I MADE BECAUSE IT IS MORE READABLE 
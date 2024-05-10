module DailyNine where
    -- Checks if the result of applying a function to all elements in a list yields only 'Nothing'.
    -- Parameters:
    --   - f: the function applied to each element in the list
    --   - xs: the list of elements
    -- Result:
    --   - 'True' if all results are 'Nothing', 'False' otherwise.
    onlyNothing :: (a -> Maybe b) -> [a] -> Bool
    onlyNothing _ [] = True
    onlyNothing f (x:xs) = 
        case f x of
            Nothing -> onlyNothing f xs
            _ -> False

    -- Retrieves the result of applying a function to the first element in a list that yields a 'Just' value.
    -- Parameters:
    --   - f: the function applied to each element in the list
    --   - xs: the list of elements
    -- Result:
    --   - 'Just' the first non-'Nothing' result, or 'Nothing' if all results are 'Nothing'.
    firstAnswer :: (a -> Maybe b) -> [a] -> Maybe b
    firstAnswer _ [] = Nothing
    firstAnswer f (x:xs) =
        case f x of
            Nothing -> firstAnswer f xs
            Just v -> Just v

    -- Retrieves all 'Just' values resulting from applying a function to a list of elements.
    -- Parameters:
    --   - f: the function applied to each element in the list
    --   - xs: the list of elements
    -- Result:
    --   - 'Just' a list of 'Just' values, or 'Nothing' if any 'Nothing' value is encountered.
    allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
    allAnswers f xs = foldr combineAnswers (Just []) (map f xs)
        where
            -- Combines two 'Maybe' lists, concatenating their contents if both are 'Just'.
            -- Parameters:
            --   - Maybe [b]: the first 'Maybe' list
            --   - Maybe [b]: the second 'Maybe' list
            -- Result:
            --   - 'Just' the concatenated list if both are 'Just', 'Nothing' otherwise.
            combineAnswers :: Maybe [b] -> Maybe [b] -> Maybe [b]
            combineAnswers (Just x) (Just acc) = Just (x ++ acc)
            combineAnswers _ _ = Nothing


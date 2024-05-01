module DailySeven where

    -- Finds the longest word in a list of strings.
    -- Parameters:
    --   - strings: the list of strings to search
    -- Result:
    --   - The longest string in the list, or an empty string if the list is empty.
    findLongest :: [String] -> String
    findLongert [] = ""
    findLongest = foldl (\curLongest str -> if length curLongest >= length str then curLongest else str) ""


    -- Checks if any element in a list of integers is larger than or equal to a given value.
    -- Parameters:
    --   - val: the value to compare against
    --   - nums: the list of integers to check
    -- Result:
    --   - True if any element in the list is larger than or equal to val, False otherwise.
    anyLarger :: Integer -> [Integer] -> Bool
    anyLarger val = foldl (\found num -> if num >= val then True else found) False


    -- Concatenates first and last names in a list of tuples into a single string.
    -- Parameters:
    --   - names: the list of tuples containing first and last names
    -- Result:
    --   - A string containing all first and last names separated by commas.
    allNames :: [(String, String)] -> String
    allNames [] = ""
    allNames (tup:tups) = foldl (\acc (first, last) -> acc ++ ", " ++ first ++ " " ++ last) (fst tup ++ " " ++ snd tup) tups


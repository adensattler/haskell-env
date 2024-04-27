module DailySix where

    -- Filters a list of words to include only those whose length is shorter than or equal to a given number.
    -- Parameters:
    --   - n: the maximum length of words to keep
    --   - words: the list of words to filter
    -- Result:
    --   - A list containing words whose length is shorter than or equal to n.
    shorterThan :: Int -> [String] -> [String]
    shorterThan n = filter (\word -> length word <= n)


    -- Removes multiples of a given number from a list of numbers.
    -- Parameters:
    --   - n: the number whose multiples should be removed
    --   - numbers: the list of numbers to filter
    -- Result:
    --   - A list containing numbers from the input list that are not multiples of n.
    removeMultiples :: Int -> [Int] -> [Int]
    removeMultiples n = filter (\num -> num `mod` n /= 0)


    -- Filters a list of Maybe values to remove the Nothings.
    -- Parameters:
    --   - list: the list of Maybe values to filter
    -- Result:
    --   - A list containing only Just values from the input list.
    onlyJust :: (Eq a) => [Maybe a] -> [Maybe a]
    onlyJust = filter (\x -> x /= Nothing)

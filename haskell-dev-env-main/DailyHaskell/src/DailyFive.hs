module DailyFive where
    import Data.Char(isLower)

    -- Calculates the products of each pair of integers in a list of pairs.
    -- Parameters:
    --   - pairs: the input list of pairs of integers
    -- Result:
    --   - A list of products, where each product is the result of multiplying
    --     the corresponding pair of integers from the input list.
    multPairs :: [(Integer, Integer)] -> [Integer]
    multPairs = map (\(x,y) -> (x*y))

    -- Generates a list of pairs where each pair consists of an integer and its square.
    -- Parameters:
    --   - nums: the input list of integers
    -- Result:
    --   - A list of pairs, where each pair contains an integer from the input list
    --     and its square.
    squareList :: [Integer] -> [(Integer, Integer)]
    squareList = map (\x -> (x, x*x))


    -- Determines whether the first character of each string in the input list is lowercase.
    -- Parameters:
    --   - strings: the input list of strings
    -- Result:
    --   - A list of Booleans, where each Boolean indicates whether the first character
    --     of the corresponding string is lowercase.
    findLowercase :: [String] -> [Bool]
    findLowercase = map (\(c:str) -> isLower c)
    
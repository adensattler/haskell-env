module WeeklyHaskellOne where

    -- Removes all occurrences of a specified character from a string
    -- Parameters:
    --   - The character to remove 'c'
    --   - Input string 'str'
    -- Result:
    --   - A new string with all instances of the character 'c' removed
    removeChar :: Char -> String -> String
    removeChar c = foldr (\x newString -> if x == c then newString else (x : newString)) ""
    -- NOTE: below is a version of removeChar that does not use foldr and passes all tests:
    -- removeChar _ "" = ""
    -- removeChar c (x:xs) = if x == c 
    --     then removeChar c xs 
    --     else (x : removeChar c xs)


    -- Removes whitespace characters (spaces, tabs, newlines, and carriage returns) from a string
    -- Parameter:
    --   - Input string 'str'
    -- Result:
    --   - A new string with all whitespace characters removed
    removeWhitespace :: String -> String
    removeWhitespace = removeChar ' ' . removeChar '\t' . removeChar '\n' . removeChar '\r'


    -- Removes punctuation characters (',' '.' '!' '?' ':' ';') from a string
    -- Parameter:
    --   - Input string 'str'
    -- Result:
    --   - A new string with all punctuation characters removed
    removePunctuation :: String -> String
    removePunctuation = removeChar ',' . removeChar '.' . removeChar '!' . removeChar '?' . removeChar ':' . removeChar ';' 


    -- Converts a string to a list of ASCII values
    -- Parameter:
    --   - Input string 'str'
    -- Result:
    --   - A list of ASCII values corresponding to the characters in the input string
    charsToAscii :: String -> [Int]
    charsToAscii [] = []
    charsToAscii (x:xs) = fromEnum x : charsToAscii xs


    -- Converts a list of ASCII values to a string
    -- Parameter:
    --   - List of ASCII values 'ascii'
    -- Result:
    --   - A string created from the ASCII values
    asciiToChars :: [Int] -> [Char]
    asciiToChars [] = []
    asciiToChars (x:xs) = toEnum x : asciiToChars xs


    -- Shifts a list of ASCII values by a specified integer value
    -- Parameters:
    --   - The shift value 'shift'
    --   - List of ASCII values 'ascii'
    -- Result:
    --   - A new list of ASCII values where each value has been increased by the shift value (modulo 128)
    shiftInts :: Integer -> [Integer] -> [Integer]
    shiftInts _ [] = []
    shiftInts shift (x:xs) = (x+shift) `mod` 128 : shiftInts shift xs


    -- Shifts characters in a string by a specified integer value in the ASCII encoding
    -- Parameters:
    --   - The shift value 'shift'
    --   - Input string 'str'
    -- Result:
    --   - A new string where each character has been shifted by the specified shift value in the ASCII encoding
    shiftMessage :: Int -> String -> String
    shiftMessage _ [] = []
    shiftMessage shift (x:xs) = toEnum ((fromEnum x + shift) `mod` 128) : shiftMessage shift xs


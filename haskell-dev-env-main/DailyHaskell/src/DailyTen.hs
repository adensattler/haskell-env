module DailyTen where
    -- Extracts all "Left" values in a list of Eithers.
    -- Parameters:
    --   - xs: the list of Eithers
    -- Result:
    --   - A list containing all values from that are of the Left type.
    allLefts :: [Either a b] -> [a]
    allLefts [] = []
    allLefts (Left x : xs) = x : allLefts xs
    allLefts (Right _ : xs) = allLefts xs


    -- Combines two Either values, preferring Left over Right.
    -- Parameters:
    --   - If the first parameter is a Left, it's returned.
    --   - If the second parameter is a Left, it's returned.
    --   - If both parameters are Rights, their values are summed and returned.
    produceStringOrSum :: (Either String Integer) -> (Either String Integer) -> (Either String Integer)
    produceStringOrSum (Left str) _ = Left str
    produceStringOrSum _ (Left str) = Left str
    produceStringOrSum (Right x) (Right y) = Right (x+y)


    -- Sums the integers in a list of Eithers, 
    -- or returns the first Left value in the list if present.
    -- Parameters:
    --   - xs: the list of Eithers
    -- Result:
    --   - If the list is empty, returns default of Right 0.
    --   - If any element in the list is Left, it is returned.
    --   - Otherwise it returns all Right values in the list summed.
    sumListOfEither :: [Either String Integer] -> (Either String Integer)
    sumListOfEither [] = Right 0
    sumListOfEither (Left str : _) = Left str
    sumListOfEither (Right x : xs) = case sumListOfEither xs of
                                        Right y -> Right (x + y)
                                        left -> left
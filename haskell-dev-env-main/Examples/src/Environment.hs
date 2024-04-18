module Environment where
    -- Maintain association between names and values


    -- Create an Empty Environment
    emptyEnv :: [(String, Bool)]
    emptyEnv = []

    -- Extend and Environment with a new Name and Value pair
    extendEnv :: (String, Bool) -> [(String, Bool)] -> [(String, Bool)]
    extendEnv (key, val) env = (key, value) : env

    -- Apply the environment
        -- look up the name in the Association
    applyEnv :: String => [(String, Bool)] -> Bool
    applyEnv _ [] = error "Variable Undefined"
    applyEnv key ((k, v) : remain) = if key==k
                                        then val
                                        else applyEnv key remain

    emptyEnv' :: p -> a
    emptyEnv' = \key  -> error " variable Undefined"
    extendEnv' = (key, value) funenv = \k -> if k == key 
                                                then value
                                                else (funenv k)

    applyEnv' k fenv = fenv k 
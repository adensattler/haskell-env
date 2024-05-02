module BoolEval where
    --Consumes String which is the program in the bool language
    -- Produce the result of 
        -- lexing
        -- parsing
        -- evaluating the program
    compileAndRun :: String -> ValueType
    compileAndRun program = evaluate (parseString program)

    -- evaluate
    --   consumes a ParseTree
    --   produces a result value of evaluating the given Parse Tree
    evaluate:: ParseTree -> ValueType
    evaluate tree = case tree of 
        (ValueNode val) -> val
        (NotNode val) -> let param = evaluate val
                            in 
                                case param of 
                                    (BoolType True) -> (BoolType False)
                                    (BoolType False) -> (BoolType True)

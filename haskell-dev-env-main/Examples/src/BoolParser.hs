module BoolParser where
    import BoolLexer 
    import BoolDefinitions 
    -- A Parser for things
    -- Is a function from Strings
    --   To List of Pairs
    -- Of Things and Strings

    -- parseString
    --  Consume a String that represents the program
    --  Produce a ParseTree 
    parseString :: String -> ParseTree 
    parseString str = parseTokens (lexString str)

    parseTokens :: [Token] -> ParseTree
    parseTokens tokens = let (tree, remTokens) = parseExpression tokens
                            in
                                case remTokens of 
                                    [] -> tree 
                                    _ -> error $ 
                                           "Error: Tokens remaining: " ++ show remTokens
    
    parseExpression :: [Token] -> (ParseTree, [Token])
    parseExpression tokens = 
        case lookAhead tokens of
            BoolValueToken val -> (ValueNode (BoolType val), accept tokens)
            IdToken idName -> (IdNode idName, accept tokens)
            NotToken -> let (expr, tokens') = parseExpression (accept tokens)
                            in
                                (NotNode expr, tokens')
            AndToken -> let (first, tokens') = parseExpression (accept tokens) 
                            in 
                                let (second, tokens'') = parseExpression tokens'
                                    in 
                                        (AndNode first second, tokens'')
            OrToken -> let (first, tokens') = parseExpression (accept tokens) 
                            in 
                                let (second, tokens'') = parseExpression tokens'
                                    in 
                                        (OrNode first second, tokens'')
            LeftParenToken -> let (expr, tokens') = parseExpression (accept, tokens)
                                in
                                    let (rightParen, tokens'') = lookAhead tokens'
                                        in 
                                            case rightParen of 
                                                RightParenToken -> (ParenthesizedNode expr, accept tokens')
                                                _ -> error "Syntax Error missing right parenthesis"
            KeywordToken "Let" -> let (id, tokens') = parseExpression (accept tokens)
                            in
                                let (be, tokens'') = parseExpression tokens'
                                    in 
                                        let (val, tokens''') = parseExpression tokens''
                                            in
                                                let (expr, tokens'''') = parseExpression tokens'''
                                                    in
                                                        (LetNode id val expr, tokens'''')
            KeywordToken "Be" -> (EmptyNode, accept tokens)
            KeywordToken "Lambda" -> let (param, tokens') = parseExpression (accept tokens)
                                        in
                                            let (body, tokens'') = parseExpression tokens'
                                                in 
                                                    (LambdaNode param body, tokens'')
            KeywordToken "Call" -> let (funName, tokens') = parseExpression (accept tokens)
                                        in 
                                            let (param, tokens'') = parseExpression (tokens')
                                                in 
                                                    (CallNode funName, param, tokens'')
            _ -> (EmptyNode, tokens)

    -- Helper functions
    lookAhead :: [Token] -> Token 
    lookAhead [] = EndToken 
    lookAhead (tok : toks ) = tok

    accept :: [Token] -> [Token]
    accept [] = error "No Remaining Tokens to Accept"
    accept (tok : toks) = toks 
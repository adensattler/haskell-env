module BoolLexer where
    -- Consume a program text and produce a Stream of Tokens (Words)
    import Data.Char 

    data Token = BoolValueToken Bool 
                | AndToken
                | OrToken
                | NotToken 
                | LeftParenToken
                | RightParenToken
                | LetToken 
                | LambdaToken
                | CallToken 
                | IdToken String 
                | KeywordToken String 
                | EndToken
                deriving (Show)

    -- lexString
    --   Consume a String with the boolean language program stored
    --   Produce a list of Tokens
    lexString :: String -> [Token]
    lexString [] = []
    lexString ('T':'r':'u':'e':remain) = BoolValueToken True : lexString remain 
    lexString ('F':'a':'l':'s':'e':remain) = BoolValueToken False : lexString remain 
    lexString ('&':'&':remain) = AndToken : lexString remain 
    lexString ('|':'|':remain) = OrToken : lexString remain 
    lexString ('!':remain) = NotToken : lexString remain 
    lexString ('(':remain) = LeftParenToken : lexString remain 
    lexString (')':remain) = RightParenToken : lexString remain 
    -- lexString (' ':remain) = lexString remain
    lexString str = case str of
        (c : cs) | isSpace c -> lexString cs 
        (c : cs) | isAlpha c -> getName str
        _  -> error "Unknown Character"
        where 
            getName st = lexName i : lexString cs 
                            where (i, cs) = span isIdChar st 
            isIdChar c = isAlpha c 
    -- lexString _ = error "Syntax Error: Unrecognized Symbol"

    lexName i = if isReservedWord i 
                    then KeywordToken i 
                    else IdToken i 
    
    isReservedWord w = elem w ["Let", "Be", "In", "Lambda"]
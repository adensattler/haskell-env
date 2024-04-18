module BinaryTreeExample where
    import Data.Char(isSpace)

    
    data Token = BoolValueToken Bool          
                    | AndToken
                    | OrToken
                    | NotToken
                    | LeftParenToken
                    | RightParenToken
                    | IdToken String
                    | KeywordToken String
                    deriving (Show)

    -- lexString
    --  Consume a String with the boolean language program stored
    --  Produce a list of Tokens
    lexString :: String => [Token]
    lexString [] = []
    lexString ('T':'r':'u':'e':remain) = BoolValueToken True : lexString remain
    lexString ('F':'a':'l':'s':'e':remain) = BoolValueToken False : lexString remain
    lexString ('&':'&':remain) = AndToken : lexString remain
    lexString ('|':'|':remain) = OrToken : lexString remain
    lexString ('!':remain) = NotToken : lexString remain
    lexString ('(':remain) = NotToken : lexString remain
    lexString (')':remain) = NotToken : lexString remain
    -- lexString (' ':remain) = lexString remain
    lexString str = case str of
        c : cs | isSpace c -> lexString cs
        c : cs | isAlpha c -> getName str
        where 
            getName st = IdToken i : lexString cs
                            where (i, cs) = span isIdChar st
            isIdChar c = isAlpha c

    lexName i = if isReservedWord i 
                    then KeyeordToken i
                    else IdToken i
    
    -- isReservedWord w = elem w ["Let", "In", "Lambda"]

    
    lexString _ = error "Syntax Error: Unrecognized Symbol"


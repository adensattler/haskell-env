module MiniRacketParser where

import Parser
import Expr
import Control.Applicative
import Error ( ErrorType ) 

parseBool :: Parser Bool
parseBool = do
        parseKeyword "true"
        return True
        <|> do
            parseKeyword "false"
            return False


-- parse binary bool operations
-- DONE TODO: implement parsing bool operations which have DONE
--   two parameters, these are 'and' and 'or'
parseBoolOp :: Parser BoolOp
parseBoolOp = do symbol "and" >> return And 
              <|> do symbol "or" >> return Or
    

-- parse math operations and return the MathOp
-- DONE TODO: Add the other math operations: *, div, mod
parseMathOp :: Parser MathOp
parseMathOp =
    do symbol "+" >> return Add
    <|> do symbol "-" >> return Sub
    <|> do symbol "*" >> return Mul
    <|> do symbol "div" >> return Div
    <|> do symbol "mod" >> return Mod
    

-- parse the comparison operations and return the corresponding  CompOp
-- DONE TODO: add the comparison operators: equal?, < 
parseCompOp :: Parser CompOp
parseCompOp = do symbol "equal?" >> return Eq
              <|> do symbol "<" >> return Lt

-- a literal in MiniRacket is true, false, or a number
-- DONE TODO: parse the literals: true, false, and numbers
literal :: Parser Value
literal = BoolValue <$> parseBool
          <|> IntValue <$> natural

-- parse a literal expression, which is just a literal
literalExpr :: Parser Expr
literalExpr = do
    LiteralExpr <$> literal


keywordList :: [String]
keywordList = ["false", "true", "not", "and", "or", "div", "mod", "equal?", "if", "let", "lambda"]

-- try to parse a keyword, otherwise it is a variable, this can be
-- used to check if the identifier we see (i.e., variable name) is
-- actually a keyword, which is not legal
parseKeyword :: String -> Parser String
parseKeyword keyword = do
    -- all keywords follow the identifier rules, so we'll use that
    name <- identifier
    if name `elem` keywordList && keyword == name
    then return name
    else failParse $ "saw " ++ name ++ ", expected " ++ keyword


-- DONE TODO: parse not expressions, note that "not" is a keyword,
-- (HINT: you should use parseKeyword)
notExpr :: Parser Expr
notExpr = do
          parseKeyword "not"
          expr <- parseExpr
          return (NotExpr expr)
-- EX output: Right (NotExpr (LiteralExpr (BoolValue True)),"")
-- parse for not and then parse for a single other expression!

-- DONE TODO: parse boolean expressions
-- a bool expression is the operator followed by one or more expressions
boolExpr :: Parser Expr
boolExpr = do 
           op <- parseBoolOp
           expr <- some parseExpr
           return (BoolExpr op expr)

-- DONE TODO: parse maths expressions
-- a math expression is the operator followed by one or more expressions
mathExpr :: Parser Expr
mathExpr = do
           op <- parseMathOp
           expr <- some parseExpr
           return (MathExpr op expr)
-- mathExpr = MathExpr <$> parseMathOp <*> parseExpr

-- a comparison expression is the comparison operator
--   followed by two expressions
compExpr :: Parser Expr
compExpr = CompExpr <$> parseCompOp <*> parseExpr <*> parseExpr

pairExpr :: Parser Expr
pairExpr = do
    expr1 <- parseExpr
    symbol "."
    PairExpr expr1 <$> parseExpr

-- note that this is syntactic sugar, cons is just replaced by a 
--    PairExpr abstract syntax tree 
consExpr :: Parser Expr 
consExpr = do 
    symbol "cons"
    expr1 <- parseExpr 
    PairExpr expr1 <$> parseExpr 

parseParens :: Parser Expr -> Parser Expr
parseParens p = do
    symbol "("
    e <- p
    symbol ")"
    return e

-- -- the main parsing function which alternates between all
-- -- the options you have for possible expressions
-- -- TODO: Add new expression types here
-- -- an atom is a literalExpr, which can be an actual literal or some other things
-- parseExpr :: Parser Expr
-- parseExpr = do
--     -- parseAtom
--     literalExpr
--     <|> parseParens notExpr
--     <|> parseParens parseExpr
--     <|> parseParens boolExpr
--     <|> parseParens mathExpr
--     <|> parseParens compExpr
--     <|> parseParens pairExpr
--     <|> parseParens consExpr
    
-- a helper function for testing parsing
--   To use simply type:
--      parseString "5" 
--   this will use the parseExpr Parser to parse the contents of str
parseString :: String -> Either ErrorType (Expr, String) 
parseString str = do 
    parse parseExpr str




-- Part 2:
-- -----------------------------------------------------------------------------------
-- Beginning of additions to MiniRacketParser.hs for Part 2 of the MiniRacketProject

-- TODO: add the additional kinds of things that can be an atom:
--   an atom is either a var, a literal, or a negated atom
parseAtom :: Parser Expr
parseAtom = do
    literalExpr <|> varExpr <|> negateAtom

-- TODO: Implement negateAtom
-- negate an atom, we actually only have one choice here. Our
-- parsing already correctly handles negative numbers, and we
-- cannot have negative boolean values. This leaves variables, 
-- but this needs to build a MathExpr which produces 0 - theVariable.
negateAtom :: Parser Expr
negateAtom = do
    symbol "-"
    atom <- parseAtom
    return (MathExpr Sub [LiteralExpr (IntValue 0), atom])

-- WACK TEST
-- parseString "(-varName)"
--Right (MathExpr Sub [VarExpr "varName"],"")
    

-- TODO: Implement varExpr
-- parse a var expression, here we need to make sure that
-- the identifier is *not* a keyword before accepting it
-- i.e., we fail the parse if it is     
varExpr :: Parser Expr
varExpr = do
    name <- identifier
    if name `elem` keywordList
        then failParse $ "invalid variable name '" ++ name ++ "'. variable names must not be a keyword."
        else return (VarExpr name)


-- DONE TODO: Implement ifExpr
-- parse an if-expression, which begins with the keyword if,
-- and is followed by three expressions
ifExpr :: Parser Expr
ifExpr = do
    parseKeyword "if"
    boolExpr <- parseExpr
    thenExpr <- parseExpr
    elseExpr <- parseExpr
    return (IfExpr boolExpr thenExpr elseExpr)


-- TODO: Implement let expressions  
-- a let expression begins with the keyword let, followed by
-- left parenthesis, then an identifier for the name 
-- to be bound, an expression to bind to that name, and a right
-- parenthesis, and then the body of the let expression
letExpr :: Parser Expr
letExpr = do
    parseKeyword "let"
    symbol "("
    var <- varExpr
    case var of
        VarExpr argname -> do
            argexpr <- parseExpr
            symbol ")"
            body <- parseExpr
            return (LetExpr argname argexpr body)
        _ -> failParse "expected variable name in let binding"


-- TODO: Add any newly added kinds of expression to be parsed here
-- the main parsing function which alternates between all 
-- the options for possible expressions
parseExpr :: Parser Expr
parseExpr = do
    parseAtom
    <|> parseParens parseAtom       -- this is just for me because it makes way more sense to put a negated var in parens
    -- consider the case: (let (x 5) (-x))
    -- either this or one just for a negated var
    <|> parseParens notExpr
    <|> parseParens boolExpr
    <|> parseParens mathExpr
    <|> parseParens parseExpr
    <|> parseParens compExpr
    <|> parseParens pairExpr
    <|> parseParens consExpr 
    <|> parseParens varExpr 
    <|> parseParens ifExpr 
    <|> parseParens letExpr 
    <|> parseParens lambdaExpr
    <|> parseParens applyExpr

-- End of additions to MiniRacketParser.hs for Part 2 of the
--   MiniRacketProject


-- Part 3:
-- -----------------------------------------------------------------------------------
-- Beginning of additions to MiniRacketParser.hs for Part 3 of the MiniRacketProject

-- TODO: Implement lambdaExpr 
-- parse a lambda expression which is a lambda, argument, 
-- and body, with proper parenthesis around it
lambdaExpr :: Parser Expr
lambdaExpr = do
    parseKeyword "lambda"
    symbol "("
    var <- varExpr
    case var of
        VarExpr varname -> do
            symbol ")"
            body <- parseExpr
            return (LambdaExpr varname body)
        _ -> failParse ("expected variable in first lambda parameter")
-- (lambda (var) expr)


--TODO: Implement applyExpr
-- This expression consists of a function which is being applied to 
--   a parameter expression.
applyExpr :: Parser Expr
applyExpr = do
    func <- parseExpr -- this should be a parseExpr because it can be either a varExpr or a lambdaExpr
    params <- parseExpr
    return (ApplyExpr func params)



-- TODO: Add lambdaExpr and applyExpr to the parseExpr function

-- End of additions to MiniRacketParser.hs for Part 3 of the
--   MiniRacketProject


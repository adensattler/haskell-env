module MiniRacketParserSpec where 

import Test.Hspec
import Parser
import Expr 
import MiniRacketParser
import Error

type ParseResult = Either ErrorType (Expr, String)

expr :: Either ErrorType (a2, b) -> a2
expr (Right (e, _)) = e 
expr (Left (SyntaxError msg)) = error msg
expr (Left (ParseError msg)) = error msg
expr (Left NoParse) = error "no matching parse"
expr _ = error "expr in MiniRacketParser.hs is not fully implemented yet..."

spec :: Spec 
spec = do 
    -- Parsing Literals 
    describe "parse literals" $ do
        it "parses number: 1235" $ 
            parseString "1235" `shouldBe` Right (LiteralExpr (IntValue 1235),"")
        it "parses negative numbers: -12235" $
            parseString "-12235" `shouldBe` Right (LiteralExpr (IntValue (-12235)), "")
        it "parses true" $
            parseString "true" `shouldBe` Right (LiteralExpr (BoolValue True), "")
        it "parses false" $
            parseString "false" `shouldBe` Right (LiteralExpr (BoolValue False), "")

    -- Parsing Op Values
    describe "parse op values" $ do
        -- Bool Ops
        it "parses and" $ 
            parse parseBoolOp "and" `shouldBe` Right (And, "")  
        it "parses or" $ 
            parse parseBoolOp "or" `shouldBe` Right (Or, "")
        -- Math Ops
        it "parses *" $ 
            parse parseMathOp "*" `shouldBe` Right (Mul, "")  
        it "parses div" $ 
            parse parseMathOp "div" `shouldBe` Right (Div, "")
        it "parses mod" $ 
            parse parseMathOp "mod" `shouldBe` Right (Mod, "")
        -- Comp Ops
        it "parses equal?" $ 
            parse parseCompOp "equal?" `shouldBe` Right (Eq, "")
        it "parses <" $ 
            parse parseCompOp "<" `shouldBe` Right (Lt, "")
    
    describe "parse not expr" $ do
        it "parses not true" $ 
            parse notExpr "not true" `shouldBe` Right (NotExpr (LiteralExpr (BoolValue True)),"")
        
        it "parses AND boolExpr alone" $ 
            parse boolExpr "and true false" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolValue True),LiteralExpr (BoolValue False)],"")

        it "parses OR boolExpr alone" $ 
            parse boolExpr "or true false" `shouldBe` Right (BoolExpr Or [LiteralExpr (BoolValue True),LiteralExpr (BoolValue False)],"")

        it "parses AND + OR boolExprs together" $ 
            parse boolExpr "and true (or false true)" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolValue True),BoolExpr Or [LiteralExpr (BoolValue False),LiteralExpr (BoolValue True)]],"")

        -- DO MATH OPS AS WELL LIKE parseString "(+ 1 2)"






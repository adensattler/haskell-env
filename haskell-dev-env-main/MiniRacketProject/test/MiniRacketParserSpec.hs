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


    -- Parsing Operations
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
    

    -- Parse Not Expressions
    describe "parse not expr" $ do
        it "parses not true" $ 
            parse notExpr "not true" `shouldBe` Right (NotExpr (LiteralExpr (BoolValue True)),"")
    

    --Parse Bool Expressions
    describe "parse bool exprs" $ do
        it "parses and boolExpr alone" $ 
            parseString "(and true false)" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolValue True),LiteralExpr (BoolValue False)],"")
        it "parses or boolExpr alone" $ 
            parseString "(or true false)" `shouldBe` Right (BoolExpr Or [LiteralExpr (BoolValue True),LiteralExpr (BoolValue False)],"")
        it "parses and + or boolExprs together" $ 
            parseString "(and true (or false true))" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolValue True),BoolExpr Or [LiteralExpr (BoolValue False),LiteralExpr (BoolValue True)]],"")


    -- Parse Comp Expressions ('equal?' and '<')
    describe "parse comp exprs" $ do
        it "parses 'equal?" $ 
            parseString "(equal? 2 2)" `shouldBe` Right (CompExpr Eq (LiteralExpr (IntValue 2)) (LiteralExpr (IntValue 2)),"")
        it "parses <" $ 
            parseString "(< 1 2)" `shouldBe` Right (CompExpr Lt (LiteralExpr (IntValue 1)) (LiteralExpr (IntValue 2)),"")


    -- Parse Math Expressions
    describe "parse math exprs" $ do
        it "parses (+ 1 2)" $ 
            parseString "(+ 1 2)" `shouldBe` Right (MathExpr Add [LiteralExpr (IntValue 1),LiteralExpr (IntValue 2)],"")
        it "parses (- 1 2)" $ 
            parseString "(- 1 2)" `shouldBe` Right (MathExpr Sub [LiteralExpr (IntValue 1),LiteralExpr (IntValue 2)],"")
        it "parses (* 1 2)" $ 
            parseString "(* 1 2)" `shouldBe` Right (MathExpr Mul [LiteralExpr (IntValue 1),LiteralExpr (IntValue 2)],"")
        it "parses (div 1 2)" $ 
            parseString "(div 1 2)" `shouldBe` Right (MathExpr Div [LiteralExpr (IntValue 1),LiteralExpr (IntValue 2)],"")
        it "parses (mod 1 2)" $ 
            parseString "(mod 1 2)" `shouldBe` Right (MathExpr Mod [LiteralExpr (IntValue 1),LiteralExpr (IntValue 2)],"")
        it "parses * (- 10 2) (+ 1 3)" $ 
            parse mathExpr  "* (- 10 2) (+ 1 3)" `shouldBe` Right (MathExpr Mul [MathExpr Sub [LiteralExpr (IntValue 10),LiteralExpr (IntValue 2)],MathExpr Add [LiteralExpr (IntValue 1),LiteralExpr (IntValue 3)]],"")
        it "parse mathExpr div (* 1 2) (+ 3 4)" $
            parse mathExpr "div (* 1 2) (+ 3 4)" `shouldBe` Right (MathExpr Div [MathExpr Mul [LiteralExpr (IntValue 1),LiteralExpr (IntValue 2)],MathExpr Add [LiteralExpr (IntValue 3),LiteralExpr (IntValue 4)]],"")


    -- Parse Var Expressions
    describe "parse var exprs" $ do
        it "parses (testVar)" $ 
            parseString "(testVar)" `shouldBe` Right (VarExpr "testVar","")
        it "parses var as keyword (throws error)" $ 
            parse varExpr "not" `shouldBe` Left (ParseError "invalid variable name 'not'. variable names must not be a keyword.")


    -- Parse Negated Variables
    describe "parse negated var exprs" $ do
        it "parses -testVar" $ 
            parseString "-testVar" `shouldBe` Right (MathExpr Sub [LiteralExpr (IntValue 0),VarExpr "testVar"],"")
        it "parses - testVar" $ 
            parseString "- testVar" `shouldBe` Right (MathExpr Sub [LiteralExpr (IntValue 0),VarExpr "testVar"],"")
        it "parses (< 5 -varName)" $ 
            parseString "(< 5 -varName)" `shouldBe` Right (CompExpr Lt (LiteralExpr (IntValue 5)) (MathExpr Sub [LiteralExpr (IntValue 0),VarExpr "varName"]),"")
        
    
    -- Parse If Expressions
    describe "parse if exprs" $ do
        it "parses (if true true true)" $ 
            parseString "(if true true true)" `shouldBe` Right (IfExpr (LiteralExpr (BoolValue True)) (LiteralExpr (BoolValue True)) (LiteralExpr (BoolValue True)),"")
        it "parses (if true true true)" $ 
            parseString "(if (equal? x 5) (true) (+ x 1))" `shouldBe` Right (IfExpr (CompExpr Eq (VarExpr "x") (LiteralExpr (IntValue 5))) (LiteralExpr (BoolValue True)) (MathExpr Add [VarExpr "x",LiteralExpr (IntValue 1)]),"")


    -- Parse Let Expressions
    describe "parse let exprs" $ do
        it "parses (let (x 5) (+ x 3))" $ 
           parseString "(let (x 5) (+ x 3))" `shouldBe` Right (LetExpr "x" (LiteralExpr (IntValue 5)) (MathExpr Add [VarExpr "x",LiteralExpr (IntValue 3)]),"")
        it "parses (let (x (+ z 5)) (+ k x))" $ 
           parseString "(let (x (+ z 5)) (+ k x))" `shouldBe` Right (LetExpr "x" (MathExpr Add [VarExpr "z",LiteralExpr (IntValue 5)]) (MathExpr Add [VarExpr "k",VarExpr "x"]),"")
       




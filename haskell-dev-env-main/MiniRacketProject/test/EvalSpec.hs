module EvalSpec where


import Test.Hspec
import Parser
import Expr
import MiniRacketParser

import Eval
import Error

type ParseResult = Either ErrorType (Expr, String)

spec :: Spec
spec = do
    describe "eval literals" $ do
        it "evaluates number: 1235" $ 
            evalString "1235" `shouldBe` Right (IntValue 1235)
        it "evaluates negative numbers: -12235" $
            evalString "-12235" `shouldBe` Right (IntValue (-12235))
        it "evaluates true" $
            evalString "true" `shouldBe` Right (BoolValue True)
        it "evaluates false" $
            evalString "false" `shouldBe` Right (BoolValue False)


    describe "eval 'not' expressions" $ do
        it "evaluates (not true)" $
            evalString "(not true)" `shouldBe` Right (BoolValue False)
        it "evaluates (not false)" $
            evalString "(not false)" `shouldBe` Right (BoolValue True)
        it "evaluates (not 5)" $
            evalString "(not 5)" `shouldBe` Left (TypeError "not <boolexpr> .. must evaluate to a bool type")
    

    describe "eval 'and' expressions " $ do
        it "evaluates (and true true)" $
            evalString "(and true true)" `shouldBe` Right (BoolValue True)
        it "evaluates (and true false)" $
            evalString "(and true false)" `shouldBe` Right (BoolValue False)
        it "evaluates (and false false)" $
            evalString "(and false false)" `shouldBe` Right (BoolValue False)
    

    describe "eval 'or' expressions " $ do
        it "evaluates (or true true)" $
            evalString "(or true true)" `shouldBe` Right (BoolValue True)
        it "evaluates (or true false)" $
            evalString "(or true false)" `shouldBe` Right (BoolValue True)
        it "evaluates (or false false)" $
            evalString "(or false false)" `shouldBe` Right (BoolValue False)


    describe "eval comp expressions" $ do
        it "evaluates (< 5 2)" $
            evalString "(< 5 2)" `shouldBe` Right (BoolValue False)
        it "evaluates (< 2 5)" $
            evalString "(< 2 5)" `shouldBe` Right (BoolValue True)
        it "evaluates (equal? 5 2)" $
            evalString "(equal? 5 2)" `shouldBe` Right (BoolValue False)
        it "evaluates (equal? 1 1)" $
            evalString "(equal? 1 1)" `shouldBe` Right (BoolValue True)


    describe "eval math expressions" $ do
        it "evaluates (+ 5 2)" $
            evalString "(+ 5 2)" `shouldBe` Right (IntValue 7)
        it "evaluates (- 5 2)" $
            evalString "(- 5 2)" `shouldBe` Right (IntValue 3)
        it "evaluates (* 5 2)" $
            evalString "(* 5 2)" `shouldBe` Right (IntValue 10)
        it "evaluates (div 6 2)" $
            evalString "(div 6 2)" `shouldBe` Right (IntValue 3)
        it "evaluates (mod 5 2)" $
            evalString "(mod 5 2)" `shouldBe` Right (IntValue 1)
    

    describe "eval if expressions" $ do
        it "evaluates (if true false true)" $
            evalString "(if true false true)" `shouldBe` Right (BoolValue False)
        it "evaluates (if false false true)" $
            evalString "(if false false true)" `shouldBe` Right (BoolValue True) 
        it "evaluates (if (equal? (mod 4 2) 0) true false)" $
            evalString "(if (equal? (mod 4 2) 0) true false)" `shouldBe` Right (BoolValue True)
        it "evaluates (if (equal? (mod 3 2) 0) true false)" $
            evalString "(if (equal? (mod 3 2) 0) true false)" `shouldBe` Right (BoolValue False)
    

    describe "eval let expressions" $ do
        it "evaluates (let (x 5) (+ x 3))" $
            evalString "(let (x 5) (+ x 3))" `shouldBe` Right (IntValue 8)
        it "evaluates (let (x 5) (let (k 10) (+ x k)))" $
            evalString "(let (x 5) (let (k 10) (+ x k)))" `shouldBe` Right (IntValue 15)
        it "evaluates (let (x 5) (let (k 10) (+ x k)))" $
            evalString "(let (x 5) (let (k 10) (+ x -k)))" `shouldBe` Right (IntValue (-5))
        -- PARENS around var (negateAtom) but this case is not handled unless 
        -- it "evaluates (let (x 5) (let (k 10) (+ x k)))" $
        --     evalString "(let (x 5) (let (k 10) (+ x (-k))))" `shouldBe` Right (IntValue (-5))
        it "evaluates (let (x 5) (+ x 3))" $
            evalString "(let (x 5) (+ x 3))" `shouldBe` Right (IntValue 8)


    describe "eval var expressions" $ do
        it "evaluates (let (x 5) (x)" $
            evalString "(let (x 5) (x))" `shouldBe` Right (IntValue 5)
        it "evaluates (let (x 5) (-x)" $
            evalString "(let (x 5) (-x))" `shouldBe` Right (IntValue (-5))
        it "evaluates x" $
            evalString "x" `shouldBe` Left (NoSymbol "variable x was not found in current env")


    describe "eval lambda expressions" $ do
        it "evaluates (lambda (x) (+ 1 1))" $
            evalString "(lambda (x) (+ 1 1))" `shouldBe` Right (ClosureValue "" "x" (MathExpr Add [VarExpr "x",LiteralExpr (IntValue 1)]) [])
        it "evaluates (lambda (x) (+ x 1))" $
            evalString "(lambda (x) (+ x 1))" `shouldBe` Right (ClosureValue "" "x" (MathExpr Add [LiteralExpr (IntValue 1),LiteralExpr (IntValue 1)]) [])
        it "evaluates (lambda (x) 1)" $
            evalString "(lambda (x) 1)" `shouldBe` Right (ClosureValue "" "x" (LiteralExpr (IntValue 1)) [])
    
    describe "eval apply expressions" $ do
        it "evaluates ((lambda (x) (+ x 1)) 1)" $
            evalString "((lambda (x) (+ x 1)) 1)" `shouldBe` Right (IntValue 2)
        it "evaluates (let (f (lambda (x) (+ x 1))) (f 2))" $
            evalString "(let (f (lambda (x) (+ x 1))) (f 2))" `shouldBe` Right (IntValue 3)
        
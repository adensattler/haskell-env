{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Eval where

import Expr
import Environment
import MiniRacketParser
import Error

import Control.Applicative
import Control.Monad
import Parser


-- we begin with an Expr, and produce a result of Either String (a, Expr). 
-- In the result, String represents an error and contains the error message, 
-- and the tuple (ValueEnv, Expr) contains the current Environment storing (name, value) bindings
-- along with the remaining expression to be evaluated
newtype Evaluator a = E { eval :: (ValueEnv, Expr) -> Either ErrorType (a, (ValueEnv, Expr)) }

-- this is the basic evaluator, it ends when our Abstract Syntax Tree (AST) is an EmptyExpr, 
--   but otherwise, we produce the environment and expr
next :: Evaluator (ValueEnv, Expr)
next = E (\parserState -> case parserState of 
            (_, EmptyExpr) -> Left $ EvalError "no more expressions"
            (env, x) -> Right ((env, x), (env, EmptyExpr)))

-- these next evaluators are primarily for error purposes, you can 
-- call them whenever you want to generate an error
evalError :: ErrorType -> Evaluator a
evalError err = E (\_ -> Left err)

evalNotImplemented :: Evaluator a
evalNotImplemented = evalError EvalNotImplemented 

-- failEval is an EvalError which is a general error for evaluation
failEval :: String -> Evaluator a
failEval msg = evalError $ EvalError msg

-- NoEval is called when there is nothing to evaluate
noEval :: Evaluator a
noEval = evalError NoEval

-- a TypeError is an error when there's a type mismatch discovered during runtime
typeError :: String -> Evaluator a
typeError msg = evalError $ TypeError msg

-- a NoSymbol error is an error when a symbol is looked up in the environment, but not found
noSymbol :: String -> Evaluator a
noSymbol msg = evalError $ NoSymbol msg


-- an evaluator is a Functor
instance Functor Evaluator where
    fmap f e = E (\expr -> case eval e expr of
        Left msg -> Left msg
        Right (v, out) -> Right (f v, out))

-- an evaluator is also an Applicative
instance Applicative Evaluator where
    -- the evaluator will return a successful evaluation with v as the result
    -- of evaluation and expr as the remaining stuff to evaluate
    pure v = E (\expr -> Right (v, expr))

    efuns <*> px = E (\expr -> case eval efuns expr of
        Left msg -> Left msg
        Right (g, out) -> eval (fmap g px) out)

-- and a Monad...
instance Monad Evaluator where
    e >>= f = E (\expr -> case eval e expr of
        Left msg -> Left msg
        Right (v, out) -> eval (f v) out)

-- and an Alternative ... 
instance Alternative Evaluator where
    empty = failEval "no matching evaluation"

    p <|> q = E (\expr -> case eval p expr of
        Left NoEval -> eval q expr
        Left (EvalError _) -> eval q expr
        -- we will pass forward the Lefts otherwise
        otherEval -> otherEval)

-- in fact we're a MonadPlus too, this helps when we don't have
-- an alternative or when simply we use pattern matching on <-, but
-- that fails, this allows the Monad to pass along the failure 
-- instead of generating a pattern-matching failure 
instance MonadPlus Evaluator
instance MonadFail Evaluator where
  fail _ = mzero


-- DONE TODO:
-- evaluate a literal
evalLiteral :: Evaluator Value
evalLiteral = do
    -- retrieve the literal value using next, and return the value
    (_, LiteralExpr x) <- next 
    return x


-- this evaluates a list of expressions and returns a list of values
-- by mapping an evaluator function (using <$>) over the list of expressions
evalListOfExprs :: ValueEnv -> [Expr] -> [Either ErrorType Value]
evalListOfExprs env exprs =
    (\expr ->
        case eval evalExpr (env, expr) of
            Right (res, _) -> Right res
            Left msg -> Left msg) <$> exprs


-- evaluates a bool expression, this first evaluates all the 
-- arguments to the bool expression and then uses calcBoolList 
-- to calculate the boolean operation over the arguments. Note that 
-- you must first use evalListOfExprs to evaluate the arguments. Then 
-- you can use calcBoolList with the right op on it
evalBoolExpr :: Evaluator Value
evalBoolExpr = do
    (env, BoolExpr op exprs) <- next
    -- DONE TODO: implement the remainder of the evaluation
    case calcBoolList op (evalListOfExprs env exprs) of
        Left err -> evalError err
        Right v -> return v


-- performs the boolean operation on Either String Values where this works on the Values
-- only if the kinds are BoolVal, otherwise we return Left
boolOpVal :: (Bool -> Bool -> Bool) -> Either ErrorType Value -> Either ErrorType Value -> Either ErrorType Value
boolOpVal boolop (Right (BoolValue v1)) (Right (BoolValue v2)) = Right $ BoolValue $ v1 `boolop` v2
boolOpVal _ _ _ = Left $ TypeError "boolean expressions require boolean values"

-- fold over the list using the op, assumes the list has one element
boolOpFold :: Foldable t =>
    (Bool -> Bool -> Bool) -> t (Either ErrorType Value) -> Either ErrorType Value
boolOpFold op = foldr1 (boolOpVal op)

-- determine which bool operation to use to fold with by examining the 
--   kind of BoolOp we were given
calcBoolList :: BoolOp -> [Either ErrorType Value] -> Either ErrorType Value
calcBoolList op lst = case op of
    And -> boolOpFold (&&) lst
    Or -> boolOpFold (||) lst

-- MAYBE DONE!!!
evalMathExpr :: Evaluator Value
evalMathExpr = do
    (env, MathExpr op exprs) <- next
    -- TODO: implement the remainder of the evaluation
    case calcMathList op(evalListOfExprs env exprs) of
        Right v -> return v
        Left err -> evalError err


-- takes two Either Values and runs the math op on them internally, producing the same type,
-- but failing if either of them is not an IntVal (which are the only things math ops work on)
mathOpVal :: (Integer -> Integer -> Integer) -> Either ErrorType Value -> Either ErrorType Value
    -> Either ErrorType Value
mathOpVal op (Right (IntValue v1)) (Right (IntValue v2)) = Right $ IntValue $ v1 `op` v2
mathOpVal _ _ _ = Left $ TypeError "math expressions require numeric values"

eqOpVal :: (Integer -> Integer -> Bool) -> Either ErrorType Value -> Either ErrorType Value
    -> Either ErrorType Value
--eqOpVal op (Right (BoolValue v1)) (Right (BoolValue v2)) = Right $ BoolValue $ v1 `op` v2 
eqOpVal op (Right (IntValue v1)) (Right (IntValue v2)) = Right $ BoolValue $ v1 `op` v2
eqOpVal _ _ _ = Left $ TypeError "comparisons must be on the same type"

-- takes a list of Either Values and combines them through the operation
mathOpFold :: Foldable t => (Integer -> Integer -> Integer) -> t (Either ErrorType Value)
    -> Either ErrorType Value
mathOpFold op = foldr1 (mathOpVal op)

mathOpFoldl :: Foldable t => (Integer -> Integer -> Integer) -> t (Either ErrorType Value)
    -> Either ErrorType Value
mathOpFoldl op = foldl1 (mathOpVal op)

addValList :: Foldable t => t (Either ErrorType Value) -> Either ErrorType Value
addValList = mathOpFold (+)

subValList :: Foldable t => t (Either ErrorType Value) -> Either ErrorType Value
subValList = mathOpFoldl (-)


-- TODO: Uncomment and Implement implement these functions to make 
--    the other the mathematic functions work

mulValList :: Foldable t => t (Either ErrorType Value) -> Either ErrorType Value
mulValList = mathOpFoldl (*)

divValList :: Foldable t => t (Either ErrorType Value) -> Either ErrorType Value
divValList = mathOpFoldl div

modValList :: Foldable t => t (Either ErrorType Value) -> Either ErrorType Value
modValList = mathOpFoldl mod


-- TODO: Add missing Math Operation types here
calcMathList :: 
    MathOp -> [Either ErrorType Value] -> Either ErrorType Value
calcMathList op lst = case op of
    Add -> addValList lst
    Sub -> subValList lst
    Mul -> mulValList lst
    Div -> divValList lst
    Mod -> modValList lst
    _ -> error "calcMathList operation Not implemented"


-- evaluates a comparison, specifically equals? and <
evalCompExpr :: Evaluator Value
evalCompExpr = do
    -- TODO: implement the remainder of the evaluation
    (env, CompExpr op e1 e2) <- next
    v1 <- case eval evalExpr (env, e1) of
        Left err -> evalError err
        Right (val, _) -> return val
    v2 <- case eval evalExpr (env, e2) of
        Left err -> evalError err
        Right (val, _) -> return val
    case calcCompExpr op (Right v1) (Right v2) of
        Left err -> evalError err
        Right v -> return v

{-
  A somewhat complicated implementation of calcCompExpr mainly because we have 
  so many monadic types that we have to deconstruct for the various cases. Left is
  always a failure, like a type, but Right can then be the different sub-types, like
  Int or Bool, and we can use equal? on int or bools, but not < on bools.  
-}
calcCompExpr :: CompOp -> Either ErrorType Value -> Either ErrorType Value -> Either ErrorType Value
calcCompExpr Eq v@(Right (IntValue _)) v'@(Right (IntValue _)) = Right $ BoolValue $ v == v'
calcCompExpr Eq v@(Right (BoolValue _)) v'@(Right (BoolValue _)) = Right $ BoolValue $ v == v'
calcCompExpr Eq (Right (PairValue (v, v'))) (Right (PairValue (v1, v1'))) =
    case (calcCompExpr Eq (Right v) (Right v1), calcCompExpr Eq (Right v') (Right v1')) of
        (Right (BoolValue True), Right (BoolValue True)) -> Right $ BoolValue True
        (Left _, _) -> Left $ TypeError "equal? can only compare values of the same type"
        (_, Left _) -> Left $ TypeError "equal? can only compare values of the same type"
        (_, _) -> Right $ BoolValue False
calcCompExpr Eq _ _ = Left $ TypeError "equal? can only compare values of the same type"
calcCompExpr Lt (Right (IntValue v)) (Right (IntValue v')) = Right $ BoolValue $ v < v'
calcCompExpr Lt _ _ = Left $ TypeError "< can only compare values of the numbers type"

-- evaluate a Not expression, which should flip the boolean result 
-- TODO: if the type is not a bool value after evaluating it, you
--   should return a type error, which can be done as follows:
-- typeError "not <boolexpr> .. must evaluate to a bool type"
evalNotExpr :: Evaluator Value
evalNotExpr = do
    (env, NotExpr expr) <- next
    case eval evalExpr (env, expr) of
        -- DONE TODO resolve the different cases to evaluate NOT
        Right (BoolValue val, _) -> return (BoolValue (not val))
        Right _ -> typeError "not <boolexpr> .. must evaluate to a bool type"
        Left err -> evalError err
        
-- evaluates a Pair
evalPairExpr :: Evaluator Value
evalPairExpr = do
    -- extract the current environment and make sure this is a Pair type
    (env, PairExpr e1 e2) <- next
    -- to evaluate a pair, we must evaluate the left and right parts of the pair
    case getValue $ eval evalExpr (env, e1) of 
        Right v1 ->
            case getValue $ eval evalExpr (env, e2) of
                Right v2 -> return $ PairValue (v1, v2)
                Left err -> evalError err
        Left err -> evalError err

-- evaluating an expression returns a value, this is the main
-- entry point for all evaluations, so we alternate between the 
-- different options. Note that depending on what you put first might
-- change how you evaluate your expressions.

-- TODO: Add evaluations for each newly added expression type here
evalExpr :: Evaluator Value
evalExpr =
    evalBoolExpr
    <|>
    evalNotExpr
    <|>
    evalCompExpr
    <|>
    evalMathExpr
    <|>
    evalLiteral
    <|>
    evalPairExpr
    -- <|>
    -- evalVar
    -- <|>
    -- evalLetExpr
    <|>
    evalIfExpr

-- parses the string then evaluates it
parseAndEval :: String -> Either ErrorType (Value, (ValueEnv, Expr))
parseAndEval str = do
    (ast, _) <- parse parseExpr str
    -- here, [] represents the empty environment
    eval evalExpr ([], ast)


-- extract the value from the result, which contains extra stuff we don't need to see
getValue :: Either ErrorType (Value, (ValueEnv, Expr)) -> Either ErrorType Value
getValue (Right (val, _ )) = Right val
getValue (Left err) = Left err


-- Consume a string, parse it, and then try to evaluate it
evalString :: String -> Either ErrorType Value
evalString = getValue . parseAndEval



-- Part 2:
-- -----------------------------------------------------------------------------------

-- TODO: Add the following to Eval.hs for Part 2 of the MiniRacketProject

-- Beginning of additions to Eval.hs for Part 2 of the MiniRacketProject

-- TODO: Add this to your Eval.hs file
-- Evaluate a let expression. This requires evaluating the
-- argument to the identifier. Once that is evaluated, we
-- bind the value to the name in a new environment, then
-- we evaluated the body with this new environment
evalLetExpr :: Evaluator Value
evalLetExpr = do
    evalNotImplemented

-- TODO: Implement evalIfExpr
-- Evaluate an if expression, this requires evaluating
-- the first expression in the if, which is the predicate (boolean value).
-- Only until this returns a value should you evaluate one
-- of the branches. You should NOT evaluate both branches,
-- just the 2nd expression if the test case returns true,
-- and the 3rd expression if the test case returns false
evalIfExpr :: Evaluator Value
evalIfExpr = do
    (env, IfExpr boolexpr trueexpr falseexpr) <- next
    case eval evalExpr (env, boolexpr) of
        Right (BoolValue val, _) -> 
            if val == True
            then 
                case eval evalExpr (env, trueexpr) of
                Right (val, _) -> return val
                Left err -> evalError err
            else
                case eval evalExpr (env, falseexpr) of
                    Right (val, _) -> return val
                    Left err -> evalError err
        Right _ -> typeError "bool exprs MUST evaluate to a bool"
        Left err -> evalError err


-- TODO: implement evaluating a Var
-- Evaluate a Var, this requires looking up the symbol
-- in the current environment. If it's there, we return
-- the value. If it's not, we generate a NoSymbol error
-- via: noSymbol $ "symbol " ++ name ++ " not found"
evalVar :: Evaluator Value
evalVar = do 
    (env, VarExpr var, _) <- next
    case Environment.lookup var env of
        Just v -> return v
        Nothing -> return NoSymbol ("variable" ++ var ++ " was not found in current env")


-- End of additions to Eval.hs for Part 2 of the MiniRacketProject
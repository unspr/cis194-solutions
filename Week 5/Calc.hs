{-# LANGUAGE TypeSynonymInstances,  FlexibleInstances #-}

module Calc where
import ExprT
import Parser
import StackVM
import qualified Data.Map.Strict as M

eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

-- res = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

evalStr :: String -> Maybe Integer
evalStr str = case parseExp ExprT.Lit ExprT.Add ExprT.Mul str of
                    Nothing -> Nothing
                    Just exp -> Just (eval exp)

-- res = evalStr "2+3 * 4"

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- res = reify $ mul (add (lit 2) (lit 3)) (lit 4)

instance Expr Integer where
    lit a = a
    add = (+)
    mul = (*)

instance Expr Bool where
    lit a = if a > 0 then True else False
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    add (MinMax a1) (MinMax a2)= lit (max a1 a2)
    mul (MinMax a1) (MinMax a2)= lit (min a1 a2)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit = Mod7 .(`mod` 7)
    add (Mod7 a1) (Mod7 a2) = lit (a1 + a2) 
    mul (Mod7 a1) (Mod7 a2) = lit (a1 * a2) 

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


instance Expr Program where
    lit a = [PushI a]
    add a1 a2 = a1 ++ a2 ++ [StackVM.Add]
    mul a1 a2 = a1 ++ a2 ++ [StackVM.Mul]


compile :: String -> Maybe Program
compile s = parseExp lit add mul s :: Maybe Program

-- res = compile "(3*-4) + 5"

class HasVars a where
    var :: String -> a

data VarExprT = Variable String
           | Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = Calc.Lit
    add = Calc.Add
    mul = Calc.Mul

instance HasVars VarExprT where
    var = Variable

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var s = (\str -> (\input -> case input of
                        (M.Map str num) -> Maybe num
                        _ -> Nothing)) s

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit num = (\str -> (\input -> case input of
                        (M.Map str num) -> Maybe num
                        _ -> Nothing)) num
    add a b = (\s -> M.Map s a + b -> Maybe a + b) 
    mul a b = (\s -> (M.Map s (a* b) -> Maybe (a * b)))  

-- withVars :: [(String, Integer)]
-- -> (M.Map String Integer -> Maybe Integer)
-- -> Maybe Integer
-- withVars vs exp = exp $ M.fromList vs



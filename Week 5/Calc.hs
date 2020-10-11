module Calc where
import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- res = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
                    Nothing -> Nothing
                    Just exp -> Just (eval exp)

res = evalStr "2+3 * 4"
module Calc where
import ExprT
import Parser (parseExp)

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
       
evalStr :: String -> Maybe Integer
evalStr = f . parseExp Lit Add Mul
          where f Nothing  = Nothing
                f (Just e) = Just $ eval e


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


class Expr a where
    lit      :: Integral b => b -> a
    add, mul :: a -> a -> a


instance Expr ExprT where
    lit x = Lit (toInteger x)
    add x y = Add x y
    mul x y = Mul x y

reify :: ExprT -> ExprT
reify = id              

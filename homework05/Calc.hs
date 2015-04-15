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

instance Expr Integer where
    lit = toInteger
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
    
instance Expr MinMax where
    lit = MinMax . toInteger
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)                     

toMod7 :: Integer -> Mod7
toMod7 = Mod7 . toInteger . (`mod` 7)
    
instance Expr Mod7 where
    lit = toMod7 . toInteger
    add (Mod7 x) (Mod7 y) = toMod7 $ x+y
    mul (Mod7 x) (Mod7 y) = toMod7 $ x*y
    
    
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer    
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

{-# LANGUAGE FlexibleInstances #-}
module CalcEx6 where
import qualified Data.Map as M
    
data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

class Expr a where
    lit      :: Integer -> a
    add, mul :: a -> a -> a
           
class (Expr a) => HasVars a where
    var :: String -> a

instance Expr VarExprT where
    lit = Lit . toInteger
    add = Add
    mul = Mul

instance HasVars VarExprT where                       
    var = Var

-- Data.Map.lookup applied to String to Integer map          
type LookupVar = M.Map String Integer -> Maybe Integer

instance HasVars LookupVar where
    var = M.lookup

instance Expr LookupVar where
    lit       = const . Just
    add x y m = case (x m, y m) of
                  (Nothing, _) -> Nothing
                  (_, Nothing) -> Nothing
                  (Just a, Just b) -> Just (a + b)
    mul x y m = case (x m, y m) of
                  (Nothing, _) -> Nothing
                  (_, Nothing) -> Nothing
                  (Just a, Just b) -> Just (a * b)
-- mauke from #haskell-beginners simplified the above as follows:
-- mauke> I didn't use monads for the Expr instance but applicative
-- mauke> instance Expr (M.Map String Integer -> Maybe Integer) where { lit = pure . pure; add = liftA2 (liftA2 (+)); mul = liftA2 (liftA2 (*)) }

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs                                                         

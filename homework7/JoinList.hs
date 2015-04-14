{-# LANGUAGE ScopedTypeVariables #-}
module JoinList where
import Data.Monoid ((<>))
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

                    
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2  = Append m jl1 jl2 where
    m = (tag jl1) <> (tag jl2)

tag :: Monoid m => JoinList m a -> m
tag (Empty) = mempty
tag (Single m _) = m
tag (Append m _ _) = m


instance Sized m => Sized (JoinList m a) where
    size Empty = 0
    size (Single m _)   = size m
    size (Append m _ _) = size m

size' :: Sized m => m -> Int
size' = getSize . size
                          
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ x)    = Just x
indexJ i (Append _ l r)
       | i < size' l     = indexJ i l
       | otherwise       = indexJ (i - size' l) r
indexJ _ _               = Nothing


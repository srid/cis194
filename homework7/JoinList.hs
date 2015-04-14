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


indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ i (Single m a)
    | inBounds i m   = Nothing
    | otherwise      = Just a
indexJ i (Append m jl1 jl2)
    | inBounds i m   = Nothing
    | otherwise      = let x = tag jl1 in
                       case x of _
                                     | inBounds i x -> indexJ i jl1
                                     | otherwise    -> indexJ (i - (getSize $ size x)) jl2
indexJ _ _ = Nothing


inBounds :: (Sized a) =>
            Int -> a -> Bool
inBounds i sz = i <= (getSize $ size sz)

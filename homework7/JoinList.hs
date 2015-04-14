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


-- We will use total nodes as the annotation. Thus leaf node has annotation value 1.                     
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ i (Single m a)
    | inSize i m   = Just a
    | otherwise    = Nothing
indexJ i (Append m jl1 jl2)
    | inSize i m   = let m1 = tag jl1 in
                     case m1 of _
                                    | inSize i m1  -> indexJ i jl1
                                    | otherwise    -> indexJ (i - (getSize $ size m1)) jl2
    | otherwise    = Nothing
indexJ _ _ = Nothing


inSize :: (Sized a) =>
            Int -> a -> Bool
inSize i sz = i < (getSize $ size sz)



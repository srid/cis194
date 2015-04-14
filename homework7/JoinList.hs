{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeSynonymInstances #-}
module JoinList where
import Data.Monoid ((<>))
import Sized
import Scrabble    
import Buffer
    
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
    size Empty = Size 0
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


dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ 0 jl            = jl
dropJ n (Append m l r)
      | n < size' l   = (dropJ n l) +++ r
      | otherwise     = dropJ (n - size' l) r
dropJ _ _             = Empty


takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ 0 jl            = Empty
takeJ n (Append m l r )
      | n < size' l   = takeJ n r
      | otherwise     = l +++ takeJ (n - size' l) r
takeJ _ jl            = jl


scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


-- Editor stub

type JoinListBuffer = JoinList (Score, Size) String

partitionHalf :: String -> (String, String)
partitionHalf s = let n = length s in
                  case n of
                    0 -> error "oopsie"
                    _ -> let m = div n 2 in
                         (take m s, drop m s)
    
instance Buffer JoinListBuffer where
    toString Empty = ""
    toString (Single _ x) = x
    toString (Append _ l r) = toString l ++ toString r

    fromString ""  = Empty
    fromString [c] = Single (score c, 1) [c]
    fromString s   = let (x, y) = partitionHalf s in
                     fromString x +++ fromString y

    line = indexJ

    replaceLine n s b = takeJ (n-1) b +++ fromString s +++ dropJ n b

    numLines = getScore . fst . tag

    value = size' . snd . tag

makeJoinListBuffer :: String -> JoinListBuffer
makeJoinListBuffer = fromString

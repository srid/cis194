module Fibonacci where
-- import Data.Stream (Stream)
    
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2) 


fibs1 :: [Integer]
fibs1 = map fib [0..]


fibs2 :: [Integer]
fibs2 = f 0 1 where
    f x y = x:(f y z) where z = x+y


data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:streamToList xs

instance Show a => Show (Stream a) where
    show s = show $ "[" ++ (f 20 s) ++ "]" where
        f 0 _           = "..."
        f n (Cons x xs) = show x ++ " " ++ f (n-1) xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f z = Cons z $ streamFromSeed f (f z)

nats :: Stream Integer
nats = streamFromSeed (+1) 0 

ruler :: Stream Integer
ruler = f 0 where
    f x = interleaveStreams (streamRepeat x) (f (x+1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) b = Cons x $ interleaveStreams b xs

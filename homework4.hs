module Homework4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs
              
fun1' :: [Integer] -> Integer
fun1' = product . map ((-)2) . filter even 


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate f
        where f n | even n = n `div` 2
                  | odd n  = 3 * n + 1



data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldl f Leaf where
    f Leaf elem                  = Node 0 Leaf elem Leaf
    f (Node h Leaf v Leaf) elem  = Node 1 (f Leaf elem) v Leaf
    f (Node h Leaf v right) elem = Node h (f Leaf elem) v right
    f (Node h left v Leaf) elem  = Node h left v (f Leaf elem)
    f (Node h left@(Node h1 _ _ _) v right@(Node h2 _ _ _)) elem =
        if h1 < h2 then
            let ins@(Node nh _ _ _) = (f left elem) in Node (maximum (h, nh+1)) ins v right else
            let ins@(Node nh _ _ _) = (f right elem) in Node (maximum (h, nh+1)) left v ins

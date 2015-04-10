{-# LANGUAGE TemplateHaskell #-}
module Golf where
import Data.List (repeat, zip, zip3, group, sort, transpose, unlines, maximumBy)
import Data.Ord (comparing)

skips :: [a] -> [[a]]
skips xs = foldr f ([] <$ xs) (zip [1..] xs)
    where f (n, e) acc = map g (zip [1..] acc) where
              g (i, x) = if mod n i == 0 then e:x else x


localMaxima :: [Integer] -> [Integer]
localMaxima a = map (\(a, b, c) -> b) $ filter (\(a, b, c) -> a < b && b > c) $ zip3 a (drop 1 a) $ drop 2 a
-- mauke on #haskell gave a much simpler solution: [ b | (a : b : c : _) <- tails xs, a < b && b > c ]

histogram :: [Integer] -> String
histogram = unlines . reverse . transpose . (f =<< g) . e 0 . freq
    where f m []              = []
          f m ((a,b):xs)      = (show a ++ "=" ++ replicate b '*' ++ replicate (m - b) ' '):f m xs
          g x                 = snd $ maximumBy (comparing snd) x
          e i []              = map (\j -> (j, 0)) [i..9]
          e i x@((a,b):xs)    = if i < a then (i, 0):e (i+1) x else (a, b):e (i+1) xs
          freq a              = [(head x, length x) | x <- group $ sort a]

-- frerich> srid: You could make 'freq' a bit more concise by defining 'freq = map (head &&& length ) . group . sort'

module Golf where
import Data.List (repeat, zip, zip3)

skips :: [a] -> [[a]]
skips xs = foldr f ([] <$ xs) (zip [1..] xs)
    where f (n, e) acc = map g (zip [1..] acc) where
              g (i, x) = if mod n i == 0 then e:x else x


localMaxima :: [Integer] -> [Integer]
localMaxima a = map (\(a, b, c) -> b) $ filter (\(a, b, c) -> a < b && b > c) $ zip3 a (drop 1 a) $ drop 2 a
-- mauke on #haskell gave a much simpler solution: [ b | (a : b : c : _) <- tails xs, a < b && b > c ]

{-# LANGUAGE TemplateHaskell #-}
module Golf where
import Data.List (repeat, zip, zip3, group, sort, transpose, unlines, maximumBy, (\\))
import Data.Ord (comparing)

skips :: [a] -> [[a]]
skips xs = foldr f ([] <$ xs) (zip [1..] xs)
    where f (n, e) acc = map g (zip [1..] acc) where
              g (i, x) = if mod n i == 0 then e:x else x


localMaxima :: [Integer] -> [Integer]
localMaxima a = map (\(a, b, c) -> b) $ filter (\(a, b, c) -> a < b && b > c) $ zip3 a (drop 1 a) $ drop 2 a
-- mauke on #haskell gave a much simpler solution: [ b | (a : b : c : _) <- tails xs, a < b && b > c ]

histogram :: [Integer] -> String
histogram h = (unlines . reverse . transpose . f) freq
    where f a     = map (\i -> show i ++ "=" ++ q i a) [0..9]
          q i a   = let n = maybe 0 id $ lookup i a in replicate n '*' ++ replicate (m-n) ' '
          m       = maximum $ map snd freq
          freq    = [(head x, length x) | x <- group $ sort h]

-- frerich> srid: You could make 'freq' a bit more concise by defining 'freq = map (head &&& length ) . group . sort'

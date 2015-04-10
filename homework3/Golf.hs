module Golf where
import Data.List (repeat, zip)

skips :: [a] -> [[a]]
skips xs = foldr f ([] <$ xs) (zip [1..] xs)
    where f (n, e) acc = map g (zip [1..] acc) where
              g (i, x) = if mod n i == 0 then e:x else x

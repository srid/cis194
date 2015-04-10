module Golf where
import Data.List (repeat, zip)

skips :: [a] -> [[a]]
skips xs = foldl f s (zip [1..] xs)
    where s = [] <$ xs
          f acc (n, e) = map g (zip [1..] acc) where
              g (i, x) = if mod n i == 0 then x ++ [e] else x

module Lecture where

import Data.List (foldr1)
import Control.Applicative

-- From http://www.seas.upenn.edu/~cis194/spring13/lectures/11-applicative2.html
-- Can you implement the following functions? Consider what each function does when f is replaced with each of the above types.
(*>)       :: Applicative f => f a -> f b -> f b
mapA       :: Applicative f => (a -> f b) -> ([a] -> f [b])
-- sequenceA  :: Applicative f => [f a] -> f [a]
-- replicateA :: Applicative f => Int -> f a -> f [a]

-- My solutions below:

fa *> fb = flip const <$> fa <*> fb

mapA f = foldr (g . f) (pure []) where
  g fa fb = (:) <$> fa <*> fb

-- sample f for the above function
readInt :: String -> Maybe [Int]
readInt = Just . map (read . g) where
  g c = c:""


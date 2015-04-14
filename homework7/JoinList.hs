module JoinList where
import Data.Monoid ((<>))    

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

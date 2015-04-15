-- From bitemyapp http://lpaste.net/356771972522180608

-- Implement Functor instances for the following datatypes:
newtype Identity a = Identity a deriving Show

data Pair a = Pair a a deriving Show

data Two a b = Two a b deriving Show

data Three a b c = Three a b c deriving Show

data Four a b c d = Four a b c d deriving Show

-- My answers to the above:                 
instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Functor Pair where
    fmap f (Pair x _) = Pair y y where y = (f x)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

-- Can you implement one for this type?
-- Why? Why not?
data Trivial = Trivial deriving Show

-- My answers, again:
-- NOPE, not of kind '* -> *', just like Int.

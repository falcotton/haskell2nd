-- 1
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf) = Leaf
    fmap g (Node l m r) = Node (fmap g l) (g m) (fmap g r)

-- 2
-- instance Functor ((->) a) where
--   -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
--   fmap g  = (.)

--3
-- instance Applicative ((->) a) where
--   -- pure :: b -> (a -> b)
--   pure = const 
--   -- <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
--   g <*> h = \x -> gx (h x)
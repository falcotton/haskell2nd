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

--4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  --fmap :: (a->b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z [g x | x <- xs]

instance Applicative ZipList where
  --pure :: a -> ZipList a
  pure x = Z (repeat x)

  --(<*>) :: ZipList (a->b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | g <- gs, x <- xs]

-- 5
-- pure id <*> x = x
-- x :: Applicative f => f a

-- pure (g x) = pure g <*> pure x
-- x :: a

-- x <*> pure y = pure (\g -> g y) <*> x
-- x :: Applicative f => f(a -> b)
-- y :: a

-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-- :t (pure (.)) = Applicative f => f((b->c)->(a->b)->a->c)
-- x :: Applicative f => f(b->c)
-- y :: Applicative f => f(a->b)
-- z :: Applicative f => f a

-- 6
-- instance Monad ((->) a) where
--   --(>>=) :: (a -> b) -> (b -> a -> c) -> a -> c
--   g >>= k = \a -> k (g a) a
--   --        \a -> c  = a -> c

-- 7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

x = Add (Var [1,2,3]) (Add (Var [2,4,6]) (Val 4))

instance Functor Expr where
  --fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var a) = Var (f a)
  fmap f (Val n) = Val n
  fmap f (Add l r) = Add (fmap f l) (fmap f r)

--stack ghci ./Chapter12/exercise.hs
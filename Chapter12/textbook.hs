data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Functor Tree where
  fmap g (Leaf x)   = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

data Expr = Val Int | Div Expr Expr
  deriving Show

-- eval :: Expr -> Int
-- eval (Val n)    = n
-- eval (Div x y)  = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just(x `div` y)

eval :: Expr -> Maybe Int
eval (Val n)    = Nothing
eval (Div x y)  = case eval x of
                    Nothing -> Nothing
                    Just n  -> case eval y of
                                Nothing -> Nothing
                                Just m  -> safediv n m
a1 = [False,True]
a2 = [[1..2],[2..3]]
a3 x y z = x*y*z
a4 a = (a,a)
a5 fx b = fx b
second' xs = head (tail xs)
 --  second :: [a] -> a

swap' (x,y) = (y,x)
 -- swap :: (a,b) -> (b,a)

pair' x y = (x,y)
 -- pair :: a -> b -> (a,b)

double' x = x * 2
 -- double :: Num a => a -> a

palindrome' xs = reverse xs == xs
 -- palindrome :: Eq a => [a] -> Bool

twice' f x = f (f x)
 -- twice :: a -> b -> c
 -- twice :: (t -> t) -> t -> t


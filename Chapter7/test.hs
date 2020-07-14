import Prelude hiding (all,any,takewhile,dropwhile)
--1
--map f (filter p xs)

--2
all :: (a -> Bool) -> [a] -> Bool
all f = and . map f

any :: (a -> Bool) -> [a] -> Bool
any f = or . map f

takewhile :: (a -> Bool) -> [a] -> [a]
takewhile _ []      = []
takewhile f (x:xs)  | f x       = x : takewhile f xs
                    | otherwise = []

dropwhile :: (a -> Bool) -> [a] -> [a]
dropwhile _ []      = []
dropwhile f (x:xs)  | f x       = dropwhile f xs
                    | otherwise = x:xs

--3
map' ::(a -> b) -> [a] -> [b] 
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs ) []

--4
dec2int :: [Int] -> Int
dec2int x = sum [w*d | (w,d) <- zip (reverse x) weight' ]
    where weight' = iterate (*10) 1

dec2int' :: [Int] -> Int
dec2int' = foldl (\x y -> x*10 + y) 0

--5
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y

--6
unfold p h t x   | p x = []
                        | otherwise = h x : unfold p h t (t x)

chop8 :: [Int] -> [[Int]]
chop8 = unfold (== []) (take 8) (drop 8)

map'' f = unfold (==[]) (f.head)  (tail) 

iterate' f  = unfold (==[]) (f.head) (tail) [1..]
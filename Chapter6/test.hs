import Prelude hiding ((^),and,concat,replicate,(!!),elem)

-- 1
fact' ::Int -> Int
fact' 0             = 1
fact' n  | n > 0    = n * fact' (n-1)

--2
sumdown' :: Int -> Int
sumdown' 0 = 0
sumdown' n = n + sumdown' (n-1)

--3
(^) :: Int -> Int -> Int
n ^ 0   = 1
n ^ m   = n * (n ^ (m-1))

--4
euclid :: Int -> Int -> Int
euclid n m  | n == m = n
            | n > m = euclid (n-m) m
            | n < m = euclid n (m-n)

--5
{-
length [1,2,3]  = 1 + length [2,3]
                = 1 + 1 + length [3]
                = 1 + 1 + 1 + length []
                = 1 + 1 + 1 + 0
                = 3

drop 3 [1,2,3,4,5]  = drop 2 [2,3,4,5]
                    = drop 1 [3,4,5]
                    = drop 0 [4,5]
                    = [4,5]

init [1,2,3]    = 1 : init [2,3]
                = 1 : 2 : init [3]
                = 1 : 2 : []
                = 1 : [2]
                = [1,2]
-}

--6
and :: [Bool] -> Bool
and []          = True
and (x:xs)      |x == True = and xs
                | otherwise = False

concat :: [[a]] -> [a]
concat []       = []
concat (x:xs)   = x ++ concat xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a = a : replicate (n-1) a

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem a []       = False
elem a (x:xs)   = if a == x then True else elem a xs

--7
merge :: Ord a => [a] -> [a] -> [a]
merge [] y          = y
merge x []          = x
merge (x:xs) (y:ys) | x < y     =  x : merge xs (y:ys)
                    | otherwise =  y : merge (x:xs) ys

--8
halve ::[a] -> ([a],[a])
halve [] = ([],[])
halve a = (take n a, drop n a)
    where n = length a `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort ( x:[] ) = [x]
msort a = merge (msort x) (msort y)
    where (x,y) = halve a

--9
sum' ::Num a =>  [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum xs

take' :: Int -> [a] -> [a]
take' 0 a       = []
take' n (x:xs)  = x : take' (n-1) xs

last' :: [a] -> a
last' (x:[])    = x
last' (x:xs)    = last' xs
import Prelude hiding (all,any,takewhile,dropwhile)
import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits   =   sum [w*b|(w,b) <- zip weights bits]
                        where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 :int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel .encode

channel :: [Bit] -> [Bit]
channel = id

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
unfold p h t x  | p x = []
                | otherwise = h x : unfold p h t (t x)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)

map'' f = unfold (==[]) (f.head)  (tail) 

iterate' f  = unfold (==[]) (f.head) (tail) [1..]

--7
parity8 :: [Bit] -> Int
parity8 bits = (sum bits) `mod` 2

addbit :: [Bit] -> [Bit]
addbit bits |  parity8 bits == 1    = bits ++ [1]
            |  otherwise            = bits ++ [0]

addparity :: [Bit] -> [Bit]
addparity =  concat . map (addbit) . chop8

rembit :: [Bit] -> [Bit]
rembit bits | parity8 origin == last bits   = origin
            | otherwise                     = error "parity check error"       
                where origin = take 8 bits

remparity :: [Bit] -> [Bit]
remparity = concat . map(rembit) . chop9

--8

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

channel' :: [Bit] -> [Bit]
channel' = tail

transmit' :: String -> String
transmit' =  decode . remparity . channel' . addparity . encode

--9
altmap :: (a -> b) -> (a -> b) -> [a] -> [b]
altmap _ _ [] = []
altmap f g (x:xs)   | (length xs) `mod` 2  == 0     = f x : altmap f g xs
                    | otherwise                     = g x : altmap f g xs        

--10
litle :: Int -> Int
litle x = if x >= 9 then x - 9 else x

luhncheck :: [Int] -> Int
luhncheck = sum . map (litle) . altmap (*1) (*2) 

luhn :: [Int] -> Bool
luhn x  | a `mod` 10 == 0   = True
        | otherwise         = False
            where a =  luhncheck x

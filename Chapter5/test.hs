import Data.Char

sumsq' n = sum [x^2 | x<-[1..n ]]

grid' x y = [(a,b)| a<-[0..x],b<-[0..y]]

square' n = [(a,b) | (a,b) <- grid' n n, a/= b ]

replicate' n a = [ a | _ <- [1..n] ] 

pyths' n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2+y^2==z^2]
pyths'' n = [ (x,y,z) | x<-[1..n], y<-[x..n], z<-[y..n], x^2+y^2==z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n],  n `mod` x == 0 ]

perfects' n = [x | x <- [1..n], x == sum (factors x) -x ]

nest' = [(x,y) | x <- [1,2,3], y <- [4,5,6] ]
nest'' =  concat [ [ (x,y) | y <- [4,5,6] ] | x <- [1,2,3] ] 

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [ v | (k',v) <- t , k == k' ]

positions' x xs = find x (zip xs [0..])

scalarproduct'  xs ys = sum [x*y |(x,y) <- zip xs ys]

let2int :: Char -> Int
let2int c   | isLower c = ord c - ord 'a'
                | isUpper c = ord c - ord 'A'

int2low :: Int -> Char
int2low n = chr (ord 'a' + n)

int2up :: Int -> Char
int2up n = chr (ord 'A' + n)

shift' :: Int -> Char -> Char
shift' n c   | isLower c = int2low a
                | isUpper c = int2up a 
                        where a = (let2int c +n) `mod` 26
--example
-- data Op = Add | Sub | Mul | Div

-- instance Show Op where
--     show Add = "+"
--     show Sub = "-"
--     show Mul = "*"
--     show Div = "/"

-- valid :: Op -> Int -> Int -> Bool
-- valid Add x y = x <= y 
-- valid Sub x y = x > y
-- valid Mul x y = x /= 1 && y /= 1 && x <= y
-- valid Div x y = y /= 0 && x `mod` y == 0

-- apply :: Op -> Int -> Int -> Int
-- apply Add x y = x + y
-- apply Sub x y = x - y
-- apply Mul x y = x * y
-- apply Div x y = x `div` y



instance Show Expr where
    show (Val n)        = show n
    show (App o l r)    = brak l ++ show o ++ brak r
                            where
                                brak (Val n)    = show n
                                brak e          = "(" ++ show e ++ ")"

testexpr = App Add (Val 1) (App Mul (Val 2)(Val 3))

values :: Expr -> [Int]
values (Val n)      = [n]
values (App _ l r)  = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r)  = [apply o x y |  x <- eval l,
                                    y <- eval r ,
                                    valid o x y]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
                where
                  yss = subs xs
                
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys) 

perms :: [a] -> [[a]]
perms []      = [[]]
perms (x:xs)  = concat (map (interleave x) (perms xs)) 

-- choices :: [a] -> [[a]]
-- choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
    elem (values e) (choices ns) && eval e == [n]

testsolution = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

split :: [a] -> [([a],[a])]
split []      = []
split [_]     = []
split (x:xs)  = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                l       <- exprs ls,
                r       <- exprs rs,
                e       <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

-- ops :: [Op]
-- ops = [Add,Sub,Mul,Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
    [e | ns' <- choices ns,e <- exprs ns', eval e == [n]]

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
                    lx      <- results ls,
                    ry      <- results rs,
                    res     <- combine' lx ry]
                    
combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

-- solutions'' :: [Int] -> Int -> [Expr]
-- solutions'' ns n =
--   [e | ns' <- choices ns, (e,m) <- results ns', m ==n]


--1
choices :: [a] -> [[a]]
choices x = [xss | xs <- subs x, xss <- perms xs]

--2
remfirst :: Eq a => a -> [a] -> [a]
remfirst _ []     = []
--remfirst a (x:xs) = if a == x then xs else x : remfirst a xs  
remfirst a (x:xs) | a == x    = xs
                  | otherwise = x: remfirst a xs

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _       = True
isChoice _ []       = False
isChoice (x:xs) ys  = elem x ys && isChoice xs (remfirst x ys)

--3
zerosplit :: [a] -> [([a],[a])]
zerosplit [] = []
zerosplit n = [([],n)] ++ split n ++ [(n,[])]

--4
exprlist :: [Int] -> [Expr]
exprlist ns = concat (map (exprs) (choices ns))

exprcount :: [Int] -> Int
exprcount ns = length (exprlist ns) 

validcount :: [Int] -> Int
validcount ns = length [x | n <- exprlist ns, x <- eval n]

--5
-- valid :: Op -> Int -> Int -> Bool
-- valid Add _ _ = True
-- valid Sub x y = True
-- valid Mul _ _ = True
-- valid Div x y = y /= 0 && x `mod` y == 0

--6
  
data Op = Add | Sub | Mul | Div | Pow
  deriving (Eq,Ord)

data Expr = Val Int | App Op Expr Expr
  deriving (Eq,Ord)

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Pow = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y 
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && x `mod` y == 0
valid Pow x y = y >= 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Pow x y = x ^ y

ops :: [Op]
ops = [Add,Sub,Mul,Div,Pow]

data Ans = Ans Int Result
  deriving Ord

instance Show Ans where
  show (Ans n x) =show n ++ ":" ++ show x

--差分だけで比較する
instance Eq Ans where
  (Ans n x) == (Ans m y) = n == m

solutions'' :: [Int] -> Int -> [Ans]
solutions'' ns n = 
  [(Ans (abs(n-m)) (e,m)) |  ns'   <- choices ns,
                             (e,m) <- results ns']

minanser :: [Ans] -> [Ans]
minanser xs = [x | x <- xs, x == a]
  where 
    a = minimum xs

main :: IO ()
main = print (minanser (solutions'' [1,3,7,10,25,50] 765))
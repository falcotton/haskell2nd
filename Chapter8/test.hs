
type Pos = (Int,Int)
type Trans = Pos -> Pos

--type Tree = (Int,[Tree])

type Pair a = (a,a)
type Assoc k v = [(k,v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v)<-t, k' == k]

data Move  = North | South | East | West
    deriving Show

move :: Move -> Pos -> Pos
move North  (x,y) = (x,y+1)
move South  (x,y) = (x,y-1)
move East   (x,y) = (x+1,y)
move West   (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves []        p = p
moves (m:ms)    p = moves ms (move m p)

rev :: Move -> Move
rev North   = South
rev South   = North
rev East    = West
rev West    = East

data Shape = Circle Float | React Float Float
    deriving Show

square :: Float -> Shape
square n = React n n 

area :: Shape -> Float
area (Circle r)     = pi * r^2
area (React a b )   = a * b

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just(m `div` n)

safehead :: [a] -> Maybe a
safehead []     = Nothing
safehead (x:xs) = Just(x)

data Nat = Zero | Succ Nat
    deriving Show

nat2int :: Nat -> Int
nat2int Zero        = 0
nat2int (Succ m)    = 1 + nat2int m

int2nat :: Int -> Nat
int2nat 0   = Zero
int2nat n   = Succ (int2nat (n-1))

add' :: Nat -> Nat -> Nat
add' Zero n     = n
add' (Succ m) n = Succ (add' m n)

data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)       = x == y
occurs x (Node l b r)   = x == b || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)        = [x]
flatten (Node l y r)    = flatten l ++ [y] ++ flatten r 

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r)  | x == y    = True
                        | x < y     = occurs' x l
                        | otherwise = occurs' x r


data Prop   = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop
            deriving Show

p1 :: Prop
p1 = And (Var 'A')(Not (Var 'A'))
p2 :: Prop
p2 = Imply (And (Var 'A')(Var 'B'))(Var 'A')
p3 :: Prop
p3 = Imply (Var 'A')(And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)    = b
eval s (Var x)      = find x s
eval s (Not p)      = not (eval s p)
eval s (And p q)    = eval s p && eval s q
eval s (Imply p q)  = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _)      = []
vars (Var p)        = [p]
vars (Not x)        = vars x
vars (And p q)      = vars p ++ vars q
vars (Imply p q)    = vars p ++ vars q

int2bin :: Int -> [Int]
int2bin 0 = []
int2bin x = [a] ++ int2bin b
    where   a = x `mod` 2
            b = x `div` 2

bools :: Int -> [[Bool]]
bools n = map (reverse . map conv .make n . int2bin) range
    where 
        range       = [0..(2^n)-1]
        make n bs   = take n (bs ++ repeat 0)
        conv 0      = False
        conv 1      = True
bools' :: Int -> [[Bool]]
bools' 0 = [[]]
bools' n = map (False:) bss ++ map (True:) bss
    where
        bss = bools' (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x:xs)   = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools' (length vs))
    where 
        vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]



data Expr = Val Int | Add Expr Expr
    deriving Show

value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y 

type Cont = [Op]
data Op = EVAL Expr | ADD Int

eval' ::Expr -> Cont -> Int
eval' (Val n)   c = exec c n
eval' (Add x y) c = eval' x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y:c) n   = eval' y (ADD n:c)
exec (ADD n:c)  m   = exec c (n+m)

value' :: Expr -> Int
value' e = eval' e []

testexpr = Add (Add (Val 2) (Val 3) ) (Val 4)

--1
mult :: Nat -> Nat -> Nat
mult Zero n         = Zero
mult (Succ n) m = add' m (mult n m)

testmult = mult (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))

--2
occurs'' :: Ord a => a -> Tree a -> Bool
occurs'' x (Leaf y)         = x == y
occurs'' x (Node l y r)     = case compare x y of
                                EQ -> True
                                LT -> occurs'' x l
                                GT -> occurs'' x r
testoccurs'' = occurs'' 1 (Node (Node (Leaf 1) 3 (Leaf 5)) 7 (Node (Leaf 8) 10 (Leaf 12)))

--3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
    deriving Show

cntleaf :: Tree' a -> Int
cntleaf (Leaf' _)   = 1
cntleaf (Node' l r) = cntleaf l + cntleaf r
 
balanced :: Tree' a -> Bool
balanced (Leaf' _)      = True
balanced (Node' l r)    = abs(cntleaf l - cntleaf r) <= 1 &&
                            balanced l && balanced r

falseTree'  = Node' (Node' (Node' (Leaf' 1) (Leaf' 2)) (Leaf' 3)) (Leaf' 5)
trueTree'   = Node' (Node' (Leaf' 2) (Leaf' 3)) (Node' (Leaf' 7) (Leaf' 9))

--4
split :: [a] -> ([a],[a])
split x = (a,b)
    where   a = take h x
            b = drop h x 
            h = (length x) `div` 2

balance :: [a] -> Tree' a 
balance [x] = Leaf' x
balance a   = Node' (balance xs) (balance ys) 
    where
        (xs,ys) = split a

--5
folde ::(Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n)   = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

--6
eval'' :: Expr -> Int
eval'' = folde (id) (\x y -> x + y) 

eval''test = (Add (Add (Val 2) (Val 5)) (Val 4))

size :: Expr -> Int
size = folde (\x -> 1) (\x y -> x + y) 

--7
-- instance Eq a => Eq (Maybe a) where
--     Just x  == Just y   = x == y
--     Nothing == Nothing  = True
--     _       == _        = False

-- instance Eq a => Eq [a] where
--     []      == []       = True
--     (x:xs)  == (y:ys)   = x == y && [xs] == [ys]
--     _       == _        = False 

--8
data Prop8  = Or Prop8 Prop8
            | Eq Prop8 Prop8

eval8 :: Subst -> Prop8 -> Bool
eval8 s (Or p q)    = eval8 s p || eval8 s q
eval8 s (Eq p q)    = eval8 s p == eval8 s q  

vars8 :: Prop8 -> [Char]
vars8 (Or p q)      = vars8 p ++ vars8 q
vars8 (Eq p q)      = vars8 p ++ vars8 q

--9
-- test9's anser = 23
test9 = Add9 (Val9 3) (Mult9 (Val9 4) (Val9 5))

data Expr9 = Val9 Int | Add9 Expr9 Expr9 | Mult9 Expr9 Expr9

-- value9 :: Expr9 -> Int
-- value9 (Val9 n)     = n
-- value9 (Add9 x y)   = value9 x + value9 y
-- value9 (Mult9 x y)  = value9 x * value9 y

type Cont9 = [Op9]
data Op9 = EVAL9 Expr9 | ADD9 Int | MULT9 Int

eval9 :: Expr9 -> Cont9 -> Int
eval9 (Val9 n) c =  exec9 c n
eval9 (Add9 x y) c =  eval9 x (EVAL9 y : c)
eval9 (Mult9 x y)  c = eval9 x (EVAL9 y : c)

exec9 :: Cont9 -> Int -> Int
exec9 [] n = n
exec9 (EVAL9 y : c) n = eval9 y (ADD9 n : c)
exec9 (ADD9 n : c) m = exec9 c (n+m)
exec9 (MULT9 n : c) m = exec9 c (n*m)

value9 :: Expr9 -> Int
value9 e = eval9 e []
import Data.Char
import Data.List
import System.IO
import System.Random hiding (next)
-- using command : stack ghci --package random ./Chapter11/exercise.hs

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
  deriving (Eq,Ord,Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
            where
              os = length (filter (== O) ps)
              xs = length (filter (== X) ps)
              ps = concat g

-- wins :: Player -> Grid -> Bool
-- wins p g = any line (rows ++ cols ++ dias)
--             where
--               line = all (== p)
--               rows = g
--               cols = transpose g
--               dias = [diag g, diag (map reverse g)]

won :: Grid -> Bool
won g = wins O g || wins X g


putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
    where
      bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside  = foldr1 (zipWith (++))
    bar     = replicate size "|"

showPlayer :: Player -> [String]
showPlayer O = ["   "," O ","   "]
showPlayer B = ["   ","   ","   "]
showPlayer X = ["   "," X ","   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = 
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where
    (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do  putStr prompt
                    xs <- getLine
                    if xs /= [] && all isDigit xs then
                      return (read xs)
                    else
                      do  putStrLn "ERROR: Invalid number"
                          getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do  cls
              goto(1,1)
              putGrid g
              run' g p

run' :: Grid -> Player -> IO ()
run' g p  | wins O g  = putStrLn "Player 0 wins\n"
          | wins X g  = putStrLn "Player X wins\n"
          | full g    = putStrLn "It's a draw!\n"
          | otherwise = 
              do  i <- getNat (prompt p)
                  case move g i p of
                    [] -> do  putStrLn "ERROR: Invalid move"
                              run' g p
                    [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr("\ESC[" ++ show y ++ ";" ++ show y ++ "H")

data Tree a = Node a [Tree a]
  deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g     = []
  | full g    = []
  | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 10

-- minimax :: Tree Grid -> Tree (Grid,Player)
-- minimax (Node g [])
--   | wins O g = Node (g,O) []
--   | wins X g = Node (g,X) []
--   | otherwise = Node (g,B) []
-- minimax (Node g ts)
--   | turn g == O = Node (g,minimum ps) ts'
--   | turn g == X = Node (g,maximum ps) ts'
--       where
--         ts' = map minimax ts
--         ps = [p | Node (_,p) _ <- ts']

-- bestmove :: Grid -> Player -> Grid
-- bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
--                   where
--                     tree = prune depth (gametree g p)
--                     Node (_,best) ts = minimax tree

-- main :: IO ()
-- main = do hSetBuffering stdout NoBuffering
--           play empty O

-- play :: Grid -> Player -> IO ()
-- play g p = do cls
--               goto (1,1)
--               putGrid g
--               play' g p

-- play' :: Grid -> Player -> IO ()
-- play' g p  
--   | wins O g  = putStrLn "Player 0 wins\n"
--   | wins X g  = putStrLn "Player X wins\n"
--   | full g    = putStrLn "It's a draw!\n"
--   | p == O    = do  i <- getNat (prompt p)
--                     case move g i p of
--                       []    -> do putStrLn "ERROR: Invalid move"
--                                   play' g p
--                       [g']  -> play g' (next p)
--   | p == X    = do  putStr "Player X is thinking... "
--                     (play $! (bestmove g p)) (next p)
  
samplegrid = [[O,B,B],[X,X,O],[X,O,B]]
sampletree = gametree samplegrid O
  
--1
treesize :: Tree a -> Int
treesize (Node _ []) = 1
treesize (Node _ ts) = 1 + sum (map treesize ts) 

treedepth :: Tree a -> Int
treedepth (Node _ []) = 0
treedepth (Node _ ts) = 1 + maximum (map treedepth ts)

--2
-- bestmove :: Grid -> Player -> [Grid]
-- bestmove g p = [g' | Node (g',p') _ <- ts, p' == best]
--                   where
--                     tree = prune depth (gametree g p)
--                     Node (_,best) ts = minimax tree

-- play' :: Grid -> Player -> IO ()
-- play' g p  
--   | wins O g  = putStrLn "Player 0 wins\n"
--   | wins X g  = putStrLn "Player X wins\n"
--   | full g    = putStrLn "It's a draw!\n"
--   | p == O    = do  i <- getNat (prompt p)
--                     case move g i p of
--                       []    -> do putStrLn "ERROR: Invalid move"
--                                   play' g p
--                       [g']  -> play g' (next p)
--   | p == X    = do  putStr "Player X is thinking... "
--                     let gs = bestmove g p
--                     rand <- randomRIO (0, length gs -1 )
--                     (play $! (gs !! rand)) (next p)

--3
-- minimax :: Tree Grid -> Tree (Grid,Player)
-- minimax (Node g [])
--   | wins O g = Node (g,O) []
--   | wins X g = Node (g,X) []
--   | otherwise = Node (g,B) []
-- minimax (Node g ts)
--   | turn g == O = Node (g,minimum ps) ts'
--   | turn g == X = Node (g,maximum ps) ts'
--       where
--         ts' = map minimax ts
--         mind = head (map mindepth ts)
--         ps = [p | Node (_,p) _ <- ts']

-- mindepth :: Tree a -> Tree a
-- mindepth (Node x ts)  = if length mints == 0 then Node x ts else head mints
--                           where
--                             mints = [tss | tss <- ts, treedepth tss == min]
--                             min   = minimum (map treedepth ts)

--4
--a
-- main :: IO ()
-- main = do hSetBuffering stdout NoBuffering
--           putStrLn "FirstPlayer:F , SecondPlayer:S"
--           putStrLn "Select player: "
--           p <- getChar
--           case p of
--             'F' -> play empty O
--             'S' -> play empty X
--             otherwise -> main 

firstmove :: Grid -> Bool
firstmove g = even moved
                where
                  moved = length ([p | p <- ps , p /= B])  
                  ps = concat g

-- minimax :: Tree Grid -> Tree (Grid,Player)
-- minimax (Node g [])
--   | wins O g = Node (g,O) []
--   | wins X g = Node (g,X) []
--   | otherwise = Node (g,B) []
-- minimax (Node g ts) = Node (g,extr ps) ts'
--       where
--         ts' = map minimax ts
--         ps = [p | Node (_,p) _ <- ts']
--         extr = if firstmove g then minimum else maximum

--b
winsize :: Int
winsize = 3

wins :: Player -> Grid -> Bool
wins p g =  any (== True) (line (rows ++ cols ++ dias))
                where
                  line = map (checkline p)
                  rows = g
                  cols = transpose g
                  dias = diagss size g ++ diagss size (map reverse g)

checkline :: Player -> [Player] -> Bool
checkline _ []      = False
checkline p (x:xs)  = if nps == wins then True else checkline p xs 
                      where
                        nps = take winsize (x:xs) 
                        wins = replicate winsize p

diags :: Grid -> Int ->  [Player]
diags g s = [g !! n !! m | n <- [0..size-1], m <- [0..size-1], n+m == s ]

diagss :: Int -> Grid -> [[Player]]
diagss s g = map (diags g) [0..s*2-1]

--c
-- bestmove ::Tree Grid ->  Grid -> Player -> Grid
-- bestmove t g p = head [g' | Node (g',p') _ <- ts, p' == best]
--                   where
--                     cts = curNode g t
--                     Node (_,best) ts = head (map minimax cts)

curNode :: Grid -> Tree Grid -> [Tree Grid]
curNode _ (Node t []) = []
curNode g (Node t ts) | t == g = [Node t ts]
                      | otherwise = concat (map (curNode g) ts)

play :: Tree Grid -> Grid -> Player -> IO ()
play t g p = do cls
                goto (1,1)
                putGrid g
                play' t g p

play' :: Tree Grid -> Grid -> Player -> IO ()
play' t g p  
  | wins O g  = putStrLn "Player 0 wins\n"
  | wins X g  = putStrLn "Player X wins\n"
  | full g    = putStrLn "It's a draw!\n"
  | p == O    = do  i <- getNat (prompt p)
                    case move g i p of
                      []    -> do putStrLn "ERROR: Invalid move"
                                  play' t g p
                      [g']  -> play t g' (next p)
  | p == X    = do  let ct = if g == empty then gametree empty X else t 
                    putStr "Player X is thinking... "
                    play ct (bestmove ct g p) (next p)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStrLn "FirstPlayer:F , SecondPlayer:S"
          putStrLn "Select player: "
          p <- getChar
          case p of
            'F' -> play (gametree empty O) empty O
            'S' -> play (gametree empty X) empty X
            otherwise -> main 


--d

minimax :: Player -> Tree Grid -> Tree (Grid,Player)
minimax p (Node g [])
  | wins O g = Node (g,O) []
  | wins X g = Node (g,X) []
  | otherwise = Node (g,B) []
minimax p (Node g ts) = Node (g, pp) [Node (gg, pp) tss]
                        where
                          Node (gg, pp) tss = alphabeta p ts

alphabeta :: Player -> [Tree Grid] -> Tree (Grid,Player)
alphabeta p (t:[]) = minimax (next p) t
alphabeta p (t:ts) = if pm == p then Node (gm, pm) tm else alphabeta p ts
                      where
                        Node (gm, pm) tm = minimax (next p) t

bestmove ::Tree Grid ->  Grid -> Player -> Grid
bestmove t g p = head [g' | Node (g',p') _ <- ts, p' == best]
                  where
                    cts = curNode g t
                    Node (_,best) ts = head (map (minimax p) cts)

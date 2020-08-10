import Prelude hiding (putStr)
import Data.Char
import System.IO hiding (putStr)
--1
putStr :: String -> IO ()
putStr xs = sequence_ [putChar x | x <- xs ]

--2
-- no edit
type Board = [Int]
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))


putBoard :: Board -> IO ()
putBoard b = putB b (length b)

putB :: Board -> Int -> IO ()
putB _ 0 = return ()
putB (x:xs) n = do  putRow x n
                    putB xs (n-1)

--3
putBoard' :: Board -> IO ()
putBoard' b = sequence_ [putRow x y | (x,y) <- zip [1..] b]

--4
newline :: IO ()
newline = putChar '\n'

adder :: IO ()
adder = do  putStr "How many numbers? "
            n <- getDigit
            newline
            ans <- add 0 n
            putStrLn ("The total is " ++ show ans)
            
add :: Int -> Int -> IO Int
add s 0 = return s
add s i = do  putStr "Enter number: "
              n <- getDigit
              newline
              add (s + n) (i - 1)

getDigit :: IO Int
getDigit = do x <- getChar
              if isDigit x then
                return (digitToInt x)
              else
                do  newline
                    putStrLn "ERROR: Invalid digit"
                    getDigit

--5
adder' :: IO ()
adder' = do putStr "How many numbers? "
            n <- getDigit
            newline
            xs <- sequence [add 0 n ]
            putStrLn ("The total is " ++ show (head xs))

--6
getCh ::IO Char
getCh = do  hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x

read' :: IO String
read' = do  x <- getCh
          if x == '\n' then
            return []
          else
            do 
                case x of
                  '\DEL' -> putStr "\b \b"
                  otherwise -> putChar x
                xs <- read'
                return (x:xs)

remdel :: String -> String ->  String
remdel xs []      = xs
remdel [] (y:ys)  = if y == '\DEL' then remdel [] ys else remdel [y] ys
remdel xs (y:ys)  | y == '\DEL' = remdel (init xs)    ys
                  | otherwise   = remdel (xs ++ [y])  ys

readLine :: IO String
readLine = do xs <- read'
              newline
              return (remdel [] xs)

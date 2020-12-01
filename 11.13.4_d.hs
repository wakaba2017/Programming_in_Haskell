import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

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
           ps = concat g
           os = length (filter (== O) ps)
           xs = length (filter (== X) ps)

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
             line = all (== p)
             rows = g
             cols = transpose g
             dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size - 1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
          where bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
            bar = replicate 3 "|"
            beside = foldr1 (zipWith (++))

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y : ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then
               [chop size (xs ++ [p] ++ ys)]
             else
               []
             where
               (xs, B : ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                     return (read xs)
                   else
                     do putStrLn "ERROR: Invalid number"
                        getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1, 1)
             putGrid g
             run' g p

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run' :: Grid -> Player -> IO ()
run' g p | wins O g  = putStrLn "Player O wins!\n"
         | wins X g  = putStrLn "Player X wins!\n"
         | full g    = putStrLn "It's a draw!\n"
         | otherwise = do i <- getNat (prompt p)
                          case move g i p of
                            [] -> do putStrLn "ERROR: Invalid move"
                                     run' g p
                            [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p | won g     = []
          | full g    = []
          | otherwise = concat [move g i p | i <- [0..((size ^ 2) - 1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

depth :: Int
depth = 9

-- 11.13.4_d --
-- アルファ・ベータ法を用いた minmax 関数に変更。
minimaxAB :: Tree Grid -> Tree (Grid, Player)
minimaxAB (Node g []) | wins O g    = Node (g, O) []
                      | wins X g    = Node (g, X) []
                      | otherwise   = Node (g, B) []
minimaxAB (Node g ts) | turn g == O = Node (g, minimum ps) ts'
                      | turn g == X = Node (g, maximum ps) ts'
                      where
                        ts' = ab_pruning (turn g) False (sortOn mydepth (map minimaxAB ts))
                        ps = [p | Node (_, p) _ <- ts']

-- 11.13.4_d --
-- アルファ・ベータ法を用いた minmax 関数に変更。
-- (mydepth は、ゲームのツリーの深さを測定する関数)
mydepth :: Tree a -> Int
mydepth (Node _ []) = 0
mydepth (Node _ ts) = 1 + maximum (map mydepth ts)

-- 11.13.4_d --
-- アルファ・ベータ法を用いた minmax 関数に変更。
-- (ab_pruning は、アルファカットとベータカットを行う関数)
ab_pruning :: Player -> Bool -> [Tree (Grid, Player)] -> [Tree (Grid, Player)]
ab_pruning p findB [] = []
ab_pruning p findB (t : ts) | p == p'   = [t]
                            | otherwise = if p' == B
                                          then t : ab_pruning p True ts
                                          else
                                            if findB == True
                                            then ab_pruning p True ts
                                            else t : ab_pruning p False ts
                            where
                              Node (_, p') _ = t

-- 11.13.4_d --
-- 通常の minmax 関数の代わりに、アルファ・ベータ法を用いた minimaxAB 関数を呼ぶように変更。
bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
               where
                 tree = prune depth (gametree g p)
                 Node (_, best) ts = minimaxAB tree

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1, 1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p | wins O g = putStrLn "Player O wins!\n"
          | wins X g = putStrLn "Player X wins!\n"
          | full g   = putStrLn "It's a draw!\n"
          | p == O   = do i <- getNat (prompt p)
                          case move g i p of
                            [] -> do putStrLn "ERROR: Invalid move"
                                     play' g p
                            [g'] -> play g' (next p)
          | p == X   = do putStr "Player X is thinking... "
                          (play $! (bestmove g p)) (next p)

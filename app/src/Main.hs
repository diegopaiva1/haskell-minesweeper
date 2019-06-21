-- @Nome      Diego Paiva e Silva
-- @Matricula 201565516AC

module Main where

import System.Environment (getArgs)
import System.Random (RandomGen, randomR, newStdGen)
import Data.List (nub)
import Data.Char (intToDigit)

-- Only board size and mines amount are expected as command line arguments
expectedArgsAmount = 2

data Board = Board {
  grid  :: [[Char]],
  mines :: [Mine]
} deriving (Eq, Show, Ord)

data Mine = Mine {
  line :: Int,
  col  :: Int
} deriving (Eq, Show, Ord)

main :: IO ()
main = do
  g    <- newStdGen
  args <- getArgs

  if (length args) /= expectedArgsAmount then
    error "Unexpected arguments amount"
  else if (read (args !! 0) :: Int) <= 0 then
    error "Argument number 1 must be greater than 0"
  else
    return()

  let input1 = read (args !! 0) :: Int
  let input2 = read (args !! 1) :: Int

  let boardSize = input1
  let minesAmount = if input2 `elem` [1..boardSize^2 `div` 2] then input2 else boardSize^2 `div` 2

  let board = makeBoard g boardSize minesAmount

  print(mines board)
  play board

makeBoard :: RandomGen g => g -> Int -> Int -> Board
makeBoard g n m = Board {
  grid  = replicate n $ replicate n '*',
  mines = nub $ makeMines g m n
}

play :: Board -> IO ()
play board = do
  printBoard board
  putStrLn "Enter line:"
  input1 <- getLine
  putStrLn "Enter column:"
  input2 <- getLine
  let line = (read input1 :: Int) - 1
  let col  = (read input2 :: Int) - 1
  if isGameOver (line + 1) (col + 1) (mines board) then
    putStrLn "Game over! Você perdeu."
  else
    play Board {
      grid  = updateMatrix (grid board) (intToDigit (nearbyMines (line + 1) (col + 1) (mines board))) (line, col),
      mines = mines board
    }

nearbyMines :: Int -> Int -> [Mine] -> Int
nearbyMines _ _ [] = 0
nearbyMines l c (x:xs)
  | ((line x == l) && (col  x == c + 1 || col  x == c - 1)) ||
    ((col  x == c) && (line x == l + 1 || line x == l - 1)) = 1 + nearbyMines l c xs
  | otherwise = nearbyMines l c xs

updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

printBoard :: Board -> IO ()
printBoard board = putStr $ unlines $ map (unwords . map show) $ grid board

isGameOver :: Int -> Int -> [Mine] -> Bool
isGameOver l c [] = False
isGameOver l c (x:xs) = if (line x == l && col x == c) then True else isGameOver l c xs

-- makeBoard :: Int -> [[Char]]
-- makeBoard n = replicate n $ replicate n '*'

makeMines :: RandomGen g => g -> Int -> Int -> [Mine]
makeMines g n s
  | n == 0 = []
  | otherwise = do
      let (i1, s2) = randomR (1, s :: Int) g
      let (i2,  _) = randomR (1, s :: Int) s2
      [Mine {col = i1, line = i2}] ++ makeMines s2 (n - 1) s

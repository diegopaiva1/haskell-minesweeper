-- @Nome      Diego Paiva e Silva
-- @Matricula 201565516AC

module Main where

import System.Environment (getArgs)
import System.Random (RandomGen, randomR, newStdGen)
import Data.List (nub)
import Data.Char (intToDigit, digitToInt)

-- Only board size and mines amount are expected as command row arguments
expectedArgsAmount = 2

data Board = Board {
  grid  :: [[Char]],
  mines :: [Mine]
} deriving (Eq, Show, Ord)

data Mine = Mine {
  row :: Char,
  col :: Int
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

makeMines :: RandomGen g => g -> Int -> Int -> [Mine]
makeMines g n s
  | n == 0 = []
  | otherwise = do
      let (i1, s2) = randomR (65, 65 + s - 1 :: Int) g
      let (i2,  _) = randomR (1, s :: Int) s2
      [Mine {row = toEnum i1, col = i2}] ++ makeMines s2 (n - 1) s

play :: Board -> IO ()
play board = do
  printBoard board
  putStrLn "Comando:"
  input <- getLine
  let (row, col) = (input !! 0, digitToInt (input !! 1) - 1)
  if isGameOver (row, col + 1) (mines board) then
    putStrLn "Game over! Você perdeu."
  else
    play (updateBoard board (intToDigit (nearbyMines (row, col + 1) (mines board))) (row, col))

updateBoard :: Board -> Char -> (Char, Int) -> Board
updateBoard board x (r, c) = Board {
  grid  = take ((fromEnum r) - 65) (grid board) ++ [take c ((grid board) !! ((fromEnum r) - 65)) ++ [x] ++ drop (c + 1) ((grid board) !! ((fromEnum r) - 65))] ++ drop (((fromEnum r) - 65) + 1) (grid board),
  mines = mines board
}

isGameOver :: (Char, Int) -> [Mine] -> Bool
isGameOver (r, c) [] = False
isGameOver (r, c) (x:xs) = if (row x == r && col x == c) then True else isGameOver (r, c) xs

nearbyMines :: (Char, Int) -> [Mine] -> Int
nearbyMines (_,_) [] = 0
nearbyMines (r, c) (x:xs)
  | (row x == r && (col x == c + 1 || col x == c - 1)) ||
    (col x == c && (fromEnum (row x) == (fromEnum r) + 1 || fromEnum (row x) == (fromEnum r) - 1)) =
    1 + nearbyMines (r, c) xs
  | otherwise = nearbyMines (r, c) xs

printBoard :: Board -> IO ()
printBoard board = putStr $ unlines $ map (unwords . map show) $ grid board

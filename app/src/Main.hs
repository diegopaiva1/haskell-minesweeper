-- @Nome      Diego Paiva e Silva
-- @Matricula 201565516AC

module Main where

import Data.List (intersperse, nub)
import Data.Char (intToDigit, digitToInt)
import System.Random (RandomGen, randomR, newStdGen)
import System.Environment (getArgs)

-- Only board size and mines amount are expected as command row arguments
expectedArgsAmount = 2

-- For the reason we have 26 letters only in the english alphabet
maxBoardSize = 26

-- For dealing with characters in range A-Z
asciiStart = 65

data Minesweeper = Minesweeper {
  board :: [[Char]],
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
  else
    return()

  let input1 = read (args !! 0) :: Int
  let input2 = read (args !! 1) :: Int

  let boardSize   = if input1 `elem` [1..maxBoardSize] then input1 else maxBoardSize
  let minesAmount = if input2 `elem` [1..boardSize^2 `div` 2] then input2 else boardSize^2 `div` 2

  let minesweeper = makeMinesweeper g boardSize minesAmount
  play minesweeper

makeMinesweeper :: RandomGen g => g -> Int -> Int -> Minesweeper
makeMinesweeper g boardSize minesAmount = Minesweeper {
  board = replicate boardSize $ replicate boardSize '*',
  mines = nub $ makeMines g boardSize minesAmount
}

makeMines :: RandomGen g => g -> Int -> Int -> [Mine]
makeMines g boardSize minesAmount
  | minesAmount == 0 = []
  | otherwise = do
      let (i1, s1) = randomR (asciiStart, asciiStart + boardSize - 1 :: Int) g
      let (i2,  _) = randomR (1, boardSize :: Int) s1
      [Mine {row = toEnum i1, col = i2}] ++ makeMines s1 boardSize (minesAmount - 1)

play :: Minesweeper -> IO ()
play minesweeper = do
  printBoard (letters (length (board minesweeper))) minesweeper
  putStrLn "\nComando:"
  input <- getLine
  let (row, col) = (input !! 0, digitToInt (input !! 1) - 1)
  if isGameOver (row, col + 1) (mines minesweeper) then
    putStrLn "Game over! Você perdeu."
  else
    play (updateBoard minesweeper (intToDigit (nearbyMines (row, col + 1) (mines minesweeper))) (row, col))

printBoard :: String -> Minesweeper -> IO ()
printBoard rowsIds minesweeper = do
  mapM_ (uncurry printRow) (zip rowsIds (board minesweeper))
  putStrLn (digits (length (board minesweeper)))

printRow :: Char -> [Char] -> IO ()
printRow c xs = putStrLn (c : ' ' : unwords (map show xs))

letters :: Int -> String
letters n
  | n == 1 = "A"
  | otherwise = letters (n - 1) ++ [toEnum (asciiStart + n - 1)]

digits :: Int -> String
digits n
  | n == 0 = ""
  | n `elem` [1..9] = digits (n - 1) ++ "   " ++ show n
  | otherwise = digits (n - 1) ++ "  " ++ show n

updateBoard :: Minesweeper -> Char -> (Char, Int) -> Minesweeper
updateBoard minesweeper x (r, c) = Minesweeper {
  board = take ((fromEnum r) - 65) (board minesweeper) ++ [take c ((board minesweeper) !! ((fromEnum r) - 65)) ++ [x] ++ drop (c + 1) ((board minesweeper) !! ((fromEnum r) - 65))] ++ drop (((fromEnum r) - 65) + 1) (board minesweeper),
  mines = mines minesweeper
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

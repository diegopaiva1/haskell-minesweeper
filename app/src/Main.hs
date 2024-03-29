{- A Haskell implementation of the Minesweeper game. Expected command-line arguments:
   (1) Board size 'n'
       Minimum board size is 1 while the maximum is 26 (for the reason we have 26 letters only in the english alphabet).
       Any value out of the range (1, 26) will start a game with the maximum board size.
   (2) Number of mines
       Minimum number of mines is 1 while the maximum is (n^2)/2. Any value out of the range (1, (n^2)/2) will start
       a game with the maximum number of mines.

   Example of a game with board size n = 4:   A  *   *   *   *
                                              B  *   *   *   *
                                              C  *   *   *   *
                                              D  *   *   *   *
                                                 1   2   3   4
-}

module Main where

import Data.List (intersperse, nub)
import Data.Char (intToDigit, digitToInt, toLower, toUpper)
import System.Exit (exitFailure, exitSuccess)
import System.Random (RandomGen, randomR, newStdGen)
import Text.Regex.PCRE
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)

expectedArgsAmount = 2
maxBoardSize = 26
ascii = 65 -- Utility for dealing with characters in range A-Z
delayMicrosseconds = 1000000

data Minesweeper = Minesweeper {board :: [[Char]], mines :: [Mine]} deriving (Eq, Show, Ord)
data Mine = Mine {row :: Char, col :: Int} deriving (Eq, Show, Ord)

main = do
  g    <- newStdGen
  args <- getArgs

  if (length args) /= expectedArgsAmount then do
    putStrLn("Error: " ++ show (length args) ++ " arguments given, exactly " ++ show expectedArgsAmount ++ " expected")
    exitFailure
  else
    return()

  let arg1 = read (args !! 0) :: Int
  let arg2 = read (args !! 1) :: Int

  let boardSize   = if arg1 `elem` [1..maxBoardSize] then arg1 else maxBoardSize
  let minesAmount = if arg2 `elem` [1..boardSize^2 `div` 2] then arg2 else boardSize^2 `div` 2

  play 1 (makeMinesweeper g boardSize minesAmount)

makeMinesweeper :: RandomGen g => g -> Int -> Int -> Minesweeper
makeMinesweeper g boardSize minesAmount = Minesweeper {
  board = replicate boardSize $ replicate boardSize '*',
  mines = makeMines g boardSize minesAmount []
}

makeMines :: RandomGen g => g -> Int -> Int -> [Mine] -> [Mine]
makeMines g boardSize minesAmount xs
  | minesAmount == 0 = []
  | otherwise = do
      let (r, s1) = randomR (ascii, ascii + boardSize - 1 :: Int) g
      let (c,  _) = randomR (1, boardSize :: Int) s1
      if not (hasMine (toEnum r, c) xs) then do
        let mine = Mine {row = toEnum r, col = c}
        mine:makeMines s1 boardSize (minesAmount - 1) (mine:xs)
      else
        makeMines s1 boardSize minesAmount xs

play :: Int -> Minesweeper -> IO ()
play turn minesweeper = do
  if allCellsOpen minesweeper then do
    putStrLn "You win!"
    exitSuccess
  else
    return()

  putStrLn ("\nTurn: " ++ show turn ++ " // Mines: " ++ show (length (mines minesweeper)) ++ " \n")
  printBoard (rowsLabels (length (board minesweeper))) (board minesweeper)
  putStrLn "\nCommand:"
  input <- getLine
  let lastRowLabel = last (rowsLabels (length (board minesweeper)))
  let actionPattern = "[+|-]?" -- '+' is for setting a cell as mine and '-' is for unsetting a cell previous set as mine
  let rowPattern = "[(a-" ++ [toLower lastRowLabel] ++ ")|(A-" ++ [lastRowLabel] ++ ")]"
  let colPattern = if length (board minesweeper) <= 9 then "([1-" ++ [last (show (length (board minesweeper)))] ++ "])" else if length (board minesweeper) `elem` [10..19] then "([1-9]|1[0-" ++ [last ([last (show (length (board minesweeper)))])] ++ "])" else "([1-9]|1[0-9]|2[0-" ++ [last (show (length (board minesweeper)))] ++ "])"
  let pattern = "^(" ++ actionPattern ++ rowPattern ++ colPattern ++ ")$"
  if input =~ pattern then do
    if head input /= '+' && head input /= '-' then do
      let (row, col) = (toUpper (input !! 0), read (if length input == 3 then [input !! 1] ++ [input !! 2] else [input !! 1]) :: Int)
      if not (open (row, col) (board minesweeper)) then
        if hasMine (row, col) (mines minesweeper) then do
          putStrLn "\nGame over! You have been exploded!"
          threadDelay delayMicrosseconds
          putStrLn "\nBoard setup:\n"
          printBoard (rowsLabels (length (board minesweeper))) (fullOpenBoard minesweeper)
        else
          play (turn + 1) (updateBoard minesweeper (intToDigit (nearbyMines (row, col) (mines minesweeper))) (row, col - 1))
      else do
        putStrLn "Invalid position - already open or marked."
        threadDelay delayMicrosseconds
        play turn minesweeper
    else do
      let (row, col) = (toUpper (input !! 1), read (if length input == 4 then [input !! 2] ++ [input !! 3] else [input !! 2]) :: Int)
      if head input == '+' then
        if not ((open (row, col) (board minesweeper))) then
          if (minesSet (board minesweeper) == (length (mines minesweeper))) then do
            putStrLn "You have already marked the maximum number of mines."
            threadDelay delayMicrosseconds
            play turn minesweeper
          else
            play (turn + 1) (updateBoard minesweeper 'B' (row, col - 1))
        else do
          putStrLn "Invalid position - already open or marked."
          threadDelay delayMicrosseconds
          play turn minesweeper
      else
        if getChar' (row, col) (board minesweeper) == 'B' then
          play (turn + 1) (updateBoard minesweeper '*' (row, col - 1))
        else do
          putStrLn "Position is not marked as mine - invalid operation."
          threadDelay delayMicrosseconds
          play turn minesweeper
  else do
    putStrLn "\nInvalid command!"
    threadDelay delayMicrosseconds
    play turn minesweeper

allCellsOpen :: Minesweeper -> Bool
allCellsOpen minesweeper
  | length (filterBoard (/= '*') (board minesweeper)) == (length (board minesweeper))^2 = True
  | otherwise = False

filterBoard :: (a -> Bool) -> [[a]] -> [a]
filterBoard f [] = []
filterBoard f (xs:xss) = filter f xs ++ filterBoard f xss

printBoard :: String -> [[Char]] -> IO ()
printBoard rowsLabels minesweeper = do
  mapM_ (uncurry printRow) (zip rowsLabels (minesweeper))
  putStrLn (colsLabels (length (minesweeper)))

printRow :: Char -> [Char] -> IO ()
printRow c xs = putStrLn ("\t\t" ++ c : ' ' : unwords (map show xs))

rowsLabels :: Int -> String
rowsLabels n
  | n == 1    = "A"
  | otherwise = rowsLabels (n - 1) ++ [toEnum (ascii + n - 1)]

colsLabels :: Int -> String
colsLabels n
  | n == 0          = "\t\t"
  | n `elem` [1..9] = colsLabels (n - 1) ++ "   " ++ show n
  | otherwise       = colsLabels (n - 1) ++ "  "  ++ show n

open :: (Char, Int) -> [[Char]] -> Bool
open (r,c) xss = if ((xss !! ((fromEnum r) - ascii)) !! (c - 1)) /= '*' then True else False

minesSet :: [[Char]] -> Int
minesSet [] = 0
minesSet (xs:xss) = (length (filter (== 'B') xs)) + minesSet xss

getChar' :: (Char, Int) -> [[Char]] -> Char
getChar' (r, c) xss = xss !! ((fromEnum r) - ascii) !! (c - 1)

updateBoard :: Minesweeper -> Char -> (Char, Int) -> Minesweeper
updateBoard minesweeper x (r, c) = do
  let row = (fromEnum r) - ascii
  Minesweeper {
  board = take (row) (board minesweeper) ++
          [take c ((board minesweeper) !! (row)) ++ [x] ++ drop (c + 1) ((board minesweeper) !! (row))] ++
          drop ((row) + 1) (board minesweeper),
  mines = mines minesweeper
}

hasMine :: (Char, Int) -> [Mine] -> Bool
hasMine (r, c) []     = False
hasMine (r, c) (x:xs) = if (row x == r && col x == c) then True else hasMine (r, c) xs

nearbyMines :: (Char, Int) -> [Mine] -> Int
nearbyMines (_,_) [] = 0
nearbyMines (r,c) (x:xs)
  | (row x == r && (col x == c + 1 || col x == c - 1)) ||
    (col x == c && (fromEnum (row x) == (fromEnum r) + 1 || fromEnum (row x) == (fromEnum r) - 1)) =
    1 + nearbyMines (r, c) xs
  | otherwise = nearbyMines (r, c) xs

fullOpenBoard :: Minesweeper -> [[Char]]
fullOpenBoard minesweeper = [[if hasMine (toEnum (i + ascii), j + 1) (mines minesweeper) then 'B' else intToDigit (nearbyMines (toEnum (i + ascii), j + 1) (mines minesweeper)) | (j,c) <- zip [0..] r] | (i,r) <- zip [0..] (board minesweeper)]

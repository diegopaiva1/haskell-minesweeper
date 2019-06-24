{- @Nome      Diego Paiva e Silva
   @Matricula 201565516AC

   A Haskell implementation of the Minesweeper game. Expected command-line arguments:
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
import System.Random (RandomGen, randomR, newStdGen)
import Text.Regex.PCRE
import System.Environment (getArgs)

expectedArgsAmount = 2
maxBoardSize = 26
asciiStart = 65 -- For dealing with characters in range A-Z

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
      let (i1, s1) = randomR (asciiStart, asciiStart + boardSize - 1 :: Int) g
      let (i2,  _) = randomR (1, boardSize :: Int) s1
      let mine = Mine {row = toEnum i1, col = i2}
      if not (hasMine mine xs) then
        mine:makeMines s1 boardSize (minesAmount - 1) (mine:xs)
      else
        makeMines s1 boardSize minesAmount xs

play :: Int -> Minesweeper -> IO ()
play turn minesweeper = do
  putStrLn ("\nTurno: " ++ show turn)
  printBoard (letters (length (board minesweeper))) minesweeper
  putStrLn "\nComando:"
  input <- getLine
  let lastRow = last (letters (length (board minesweeper)))
  let action = "[+|-]?"
  let row = "[(a-" ++ [toLower lastRow] ++ ")|(A-" ++ [lastRow] ++ ")]"
  let col = if length (board minesweeper) <= 9 then "([1-" ++ [last (show (length (board minesweeper)))] ++ "])" else if length (board minesweeper) `elem` [10..19] then "([1-9]|1[0-" ++ [last ([last (show (length (board minesweeper)))])] ++ "])" else "([1-9]|1[0-9]|2[0-" ++ [last (show (length (board minesweeper)))] ++ "])"
  let pattern = "^(" ++ action ++ row ++ col ++ ")$"
  if input =~ pattern then do
    let openMinePattern  = "^(" ++ row ++ col ++ ")$"
    if input =~ openMinePattern then do
      let (row, col) = (toUpper (input !! 0), read (if length input == 3 then [input !! 1] ++ [input !! 2] else [input !! 1]) :: Int)
      if not (mineSet (row, col) (board minesweeper)) then
        if hasMine (Mine {row = fst (row, col), col = snd (row, col)}) (mines minesweeper) then
          putStrLn "Game over! Você perdeu."
        else
          play (turn + 1) (updateBoard minesweeper (intToDigit (nearbyMines (row, col) (mines minesweeper))) (row, col - 1))
      else do
        putStrLn "Posição está marcada como mina."
        play turn minesweeper
    else do
      let (row, col) = (toUpper (input !! 1), read (if length input == 4 then [input !! 2] ++ [input !! 3] else [input !! 2]) :: Int)
      play (turn + 1) (updateBoard minesweeper (if input !! 0 == '+' then 'B' else if mineSet (row, col) (board minesweeper) then '*' else getChar' (row, col) (board minesweeper)) (row, col - 1))
  else do
    putStrLn "Comando inválido!"
    play turn minesweeper

printBoard :: String -> Minesweeper -> IO ()
printBoard rowsIds minesweeper = do
  mapM_ (uncurry printRow) (zip rowsIds (board minesweeper))
  putStrLn (digits (length (board minesweeper)))

printRow :: Char -> [Char] -> IO ()
printRow c xs = putStrLn ("\t\t" ++ c : ' ' : unwords (map show xs))

letters :: Int -> String
letters n
  | n == 1    = "A"
  | otherwise = letters (n - 1) ++ [toEnum (asciiStart + n - 1)]

digits :: Int -> String
digits n
  | n == 0          = "\t\t"
  | n `elem` [1..9] = digits (n - 1) ++ "   " ++ show n
  | otherwise       = digits (n - 1) ++ "  "  ++ show n

mineSet :: (Char, Int) -> [[Char]] -> Bool
mineSet (r,c) xss = if ((xss !! ((fromEnum r) - asciiStart)) !! (c - 1)) == 'B' then True else False

getChar' :: (Char, Int) -> [[Char]] -> Char
getChar' (r, c) xss = xss !! ((fromEnum r) - asciiStart) !! (c - 1)

updateBoard :: Minesweeper -> Char -> (Char, Int) -> Minesweeper
updateBoard minesweeper x (r, c) = Minesweeper {
  board = take ((fromEnum r) - asciiStart) (board minesweeper) ++ [take c ((board minesweeper) !! ((fromEnum r) - asciiStart)) ++ [x] ++ drop (c + 1) ((board minesweeper) !! ((fromEnum r) - asciiStart))] ++ drop (((fromEnum r) - asciiStart) + 1) (board minesweeper),
  mines = mines minesweeper
}

hasMine :: Mine -> [Mine] -> Bool
hasMine mine []     = False
hasMine mine (x:xs) = if (row x == row mine && col x == col mine) then True else hasMine mine xs

nearbyMines :: (Char, Int) -> [Mine] -> Int
nearbyMines (_,_) [] = 0
nearbyMines (r,c) (x:xs)
  | (row x == r && (col x == c + 1 || col x == c - 1)) ||
    (col x == c && (fromEnum (row x) == (fromEnum r) + 1 || fromEnum (row x) == (fromEnum r) - 1)) =
    1 + nearbyMines (r, c) xs
  | otherwise = nearbyMines (r, c) xs

-- could write 'module Main where'

import Board
import Control.Monad
import Text.Read
import System.Random
import Data.List.Extra (nubOrd)

main :: IO ()
main = do
  board <- boardGenerate 8 8 10
  putStrLn "Welcome to minesweeper. Board size: 8x8"
  putStrLn $ format board
  update board

boardGenerate :: Int -> Int -> Int -> IO Board
boardGenerate rows cols bombs = do
  generator <- newStdGen
  return $ randomBoard generator rows cols bombs

randomBoard :: StdGen -> Int -> Int -> Int -> Board
randomBoard gen rows cols bombs =
  makeBoard rows cols $ take bombs $ nubOrd $
    zip (randomRs (1, rows) g1 :: [Int]) (randomRs (1, cols) g2 :: [Int])
  where (g1, g2) = split gen

update :: Board -> IO ()
update board = do
  userInput <- getLine
  let result = readMaybe userInput :: Maybe (Int, Int)
  newboard <- case result of
    Nothing -> do
      putStrLn ("parse error: " ++ show userInput ++ " not a tuple")
      return board
    Just (row, col) -> do
      let newboard = openCell row col board
      return newboard
  putStrLn $ format newboard
  if checkWin newboard
    then putStrLn "You win"
    else if checkLose newboard
          then putStrLn "You lose"
          else update newboard

-- could write 'module Main where'

import Board
import Control.Monad
import Text.Read

main :: IO ()
main = do
  let board = makeBoard 2 2 [(1,1)]
  putStrLn "Welcome to minesweeper. Board size: 8x8"
  update board

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

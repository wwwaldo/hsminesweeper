-- could write 'module Main where'

import Board
import Control.Monad
import Text.Read

main :: IO ()
main = do
  let board = makeBoard 8 8 [(1,1)]
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
  update newboard

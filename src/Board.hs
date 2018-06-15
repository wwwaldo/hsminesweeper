module Board where

import Data.Array.IArray
import Data.List

data Board =
  Board (Array (Int, Int) Bool)
  deriving(Show)

format :: Board -> String
format board@(Board array) =
  let
      (rows, cols) = snd $ bounds array
      horizontalborder = replicate (2 * cols + 1) '#'
      cells = map (\ y -> formatRow y board  ++ "|") [1..rows]

      separator = replicate (2 * cols + 1) '-'

      stuff :: [String]
      stuff = intersperse separator cells
  in unlines ([horizontalborder] ++ stuff ++ [horizontalborder])

-- format the row belonging to board
formatRow :: Int -> Board -> String
formatRow row board@(Board array) = case row of
  row -> concat $ map formatter [1 .. width board]
  where
    width :: Board -> Int
    width (Board array) = snd $ snd $ bounds array

    formatter :: Int -> String
    formatter x = "|" ++ q
      where q = if array!(row, x) == False then " " else "."

-- index board coordinates from 1
makeBoard :: Int -> Int -> Board
makeBoard rows cols = Board $ listArray ((1, 1), (rows, cols)) (repeat False)

openCell :: Int -> Int -> Board -> Board
openCell row col board@(Board array) = Board $ array // [((row, col), True)]

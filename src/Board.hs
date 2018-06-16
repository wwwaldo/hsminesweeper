module Board where

import Data.Array.IArray
import Data.List

data BoardState =
  Unknown
  | Bomb
  | VisibleBomb
  | Warning Int
  deriving(Show, Eq)

data Board =
  Board (Array (Int, Int) BoardState)
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
      where q = case array!(row, x) of
                  Unknown -> " "
                  Bomb -> " "
                  Warning 0 -> "."
                  Warning n -> show n
                  VisibleBomb -> "*"

-- index board coordinates from 1
makeBoard :: Int -> Int -> [(Int, Int)]-> Board
makeBoard rows cols bombLocs = Board filledArray
  where initArray = listArray ((1, 1), (rows, cols)) (repeat Unknown)
        filledArray = initArray // (map (flip (,) Bomb) bombLocs)

openCell :: Int -> Int -> Board -> Board
openCell row col board@(Board array) = Board $ array // [((row, col), update oldState)]
  where
    oldState = array!(row, col)
    update VisibleBomb = VisibleBomb
    update (Warning n) = Warning n
    update Bomb = VisibleBomb
    update Unknown = Warning 0

checkWin :: Board -> Bool
checkWin (Board array) = all (\x -> x /= VisibleBomb && x /= Unknown) (elems array)

checkLose :: Board -> Bool
checkLose (Board array) = any (==VisibleBomb) (elems array)

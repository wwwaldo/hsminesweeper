module Board where

import Data.Array.IArray
import Data.List

type Flag = Bool

data BoardState =
  Unknown Flag
  | Bomb Flag
  | VisibleBomb
  | Warning Int
  deriving(Show, Eq)

data Board =
  Board (Array (Int, Int) BoardState)
  deriving(Show)

data BoardWithStack =
  BoardWithStack [(Int, Int)] Board deriving (Show)

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
                  Unknown True-> "F"
                  Unknown False -> " "
                  Bomb True -> "F"
                  Bomb False -> " "
                  Warning 0 -> "."
                  Warning n -> show n
                  VisibleBomb -> "*"

-- index board coordinates from 1
makeBoard :: Int -> Int -> [(Int, Int)]-> Board
makeBoard rows cols bombLocs = Board filledArray
  where initArray = listArray ((1, 1), (rows, cols)) (repeat (Unknown False))
        filledArray = initArray // (map (flip (,) (Bomb False)) bombLocs)

addFlag :: Int -> Int -> Board -> Board
addFlag row col board = setState newState (row, col) board
  where newState = case (getState (row, col) board) of
                    Unknown _ -> Unknown True
                    Bomb _ -> Bomb True
                    x -> x

removeFlag :: Int -> Int -> Board -> Board
removeFlag row col board = setState newState (row, col) board
  where newState = case (getState (row, col) board) of
                    Unknown _ -> Unknown False
                    Bomb _ -> Bomb False
                    x -> x

openCell :: Int -> Int -> Board -> Board
openCell row col board = update oldState
  where
    oldState = getState (row, col) board
    (BoardWithStack _ newBoard) = openRecursive (BoardWithStack [(row, col)] board)
    update (Unknown _) = newBoard
    update (Bomb _) = setState VisibleBomb (row, col) board
    update _ = board

getState :: (Int, Int) -> Board -> BoardState
getState l (Board array) = array!l

setState :: BoardState -> (Int, Int) -> Board -> Board
setState state l (Board array) = Board $ array // [(l, state)]

-- only call if elems have Unknown -> Warning n
openRecursive :: BoardWithStack -> BoardWithStack
openRecursive bws@(BoardWithStack [] _) = bws
openRecursive (BoardWithStack (l:ls) board)
  | Warning _ <- getState l board = openRecursive $ BoardWithStack ls board
  | otherwise = openRecursive $ BoardWithStack newStack newBoard
  where
    cellNeighbors = filter (\x -> getState x board /= (Warning 0)) (neighbors l board)
    numBombNeighbors = length $ filter isBomb $ map (\l' -> getState l' board) cellNeighbors
    newBoard = setState (Warning numBombNeighbors) l board
    nbrs = if (numBombNeighbors == 0) then cellNeighbors else []
    newStack = nbrs ++ ls

neighbors :: (Int, Int) -> Board -> [(Int, Int)]
neighbors (row, col) board = filter isValid candidates
  where
    candidates = [(r, c) | r <- [row-1..row+1], c <- [col-1..col+1], (r, c) /= (row, col)]
    isValid (r, c) = r >= 1 && c >= 1 && r <= maxRow && c <= maxCol
    (maxRow, maxCol) = boardBounds board

boardBounds :: Board -> (Int, Int)
boardBounds (Board array) = snd $ bounds array

hasFlag :: BoardState -> Maybe Bool
hasFlag (Unknown True) = Just True
hasFlag (Bomb True) = Just True
hasFlag (Unknown False) = Just False
hasFlag (Bomb False) = Just False
hasFlag _ = Nothing

isUnknown :: BoardState -> Bool
isUnknown (Unknown _) = True
isUnknown _ = False

isBomb :: BoardState -> Bool
isBomb (Bomb _) = True
isBomb _ = False

checkWin :: Board -> Bool
checkWin (Board array) = all (\x -> x /= VisibleBomb && not (isUnknown x)) (elems array)

checkLose :: Board -> Bool
checkLose (Board array) = any (== VisibleBomb) (elems array)

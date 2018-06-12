module Board where

data Board =
  Board Int Int

format :: Board -> String
format (Board rows cols) =
  let horizontalborder = replicate (2 * cols + 1) '#'
      cells = concat (replicate cols "| ")  ++ "|"
      separator = replicate (2 * cols + 1) '-'

      stuff :: [String]
      stuff = init $ concat $ replicate rows [cells, separator]
  in unlines ([horizontalborder] ++ stuff ++ [horizontalborder])

makeBoard :: Int -> Int -> Board
makeBoard rows cols = Board rows cols

openCell :: Int -> Int -> Board -> Board
openCell x y board = board

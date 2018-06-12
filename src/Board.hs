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
  in
  unlines ([horizontalborder] ++ stuff ++ [horizontalborder])



makeBoard :: Int -> Int -> Board
makeBoard rows cols = Board rows cols

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

myReplicate :: Int -> a -> [a]
myReplicate n a = case n of
  0 -> []
  n | n < 0 -> []
  _ -> a : myReplicate (n - 1) a

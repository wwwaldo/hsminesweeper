-- could write 'module Main where'

import System.Environment
import Prelude hiding (forever)

main :: IO ()
-- main = do
--   args <- getArgs
--   -- putStrLn $ show args
--   let commands :: [IO ()]
--       commands = map print args
--   _

-- - what is do notation
-- - why does map not work?

main = do
  args <- getArgs
  print args
  let len = length args
      x = 4 + y
      y = x
  if len == 0
    then error "please give arguments"
    else do
      print args
      -- print x
  let var = 2
  return ()
  putStrLn "Done"




foo =
  getArgs >>= \ args ->
   print args >>
   let len = (length args)
   in if len == 0 then error "please give arguments" else print args

nTimes :: Int -> IO () -> IO ()
nTimes n command = case n of
  n | n > 0 -> nTimes (n - 1) command >> command
  _ -> return ()


command :: IO ()
command = print "hi"

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module BoardSpec where

import Test.Hspec
import Board

spec = do
  it "allows to open cells on a board" $ do
    let board = makeBoard 2 2
        newBoard = openCell 0 0 board
        expected = unlines
          [
          "#####",
          "|.| |",
          "-----",
          "| | |",
          "#####"
          ]
    format newBoard `shouldBe` expected


  it "shows initial board" $ do
    let board = makeBoard 2 2
        expected = unlines
          [
          "#####",
          "| | |",
          "-----",
          "| | |",
          "#####"
          ]
    format board `shouldBe` expected

  it "shows board of different size" $ do
    let board = makeBoard 1 1
        expected = unlines
          [
          "###",
          "| |",
          "###"
          ]
    format board `shouldBe` expected

  it "foo" $ do
    putStrLn $ display True
    putStrLn $ display (4 :: Int)
    displayToStdOut (4 :: Int)
    print (defaultValue :: Int)

-- todo
-- - typeclasses
-- - IO

class Display a where
  display :: a -> String

instance Show a => Display a where
  display = show

displayToStdOut :: Display a => a -> IO ()
displayToStdOut = putStrLn . display

x = 5

f = \ x -> x + 2

whiteList :: () -> [String]
whiteList () = ["127.0.0.1", "localhost"]


class Def a where
  defaultValue :: a

instance Def Bool where
  defaultValue = False

instance Def Int where
  defaultValue = 42

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module BoardSpec where

import Test.Hspec
import Board

spec = do
  it "shows initial board" $ do
    let board = makeBoard 2 2 []
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
    let board = makeBoard 1 1 []
        expected = unlines
          [
          "###",
          "| |",
          "###"
          ]
    format board `shouldBe` expected

  it "allows to open cells on a board" $ do
    let board = makeBoard 2 2 []
        newBoard = openCell 1 1 board
        expected = unlines
          [
          "#####",
          "|.| |",
          "-----",
          "| | |",
          "#####"
          ]
    format newBoard `shouldBe` expected

  it "displays bombs on a board" $ do
    let board = makeBoard 2 2 [(1, 1)]
        newBoard = openCell 1 1 $ openCell 1 2 board
        expected = unlines
          [
          "#####",
          "|*|.|",
          "-----",
          "| | |",
          "#####"
          ]
    format newBoard `shouldBe` expected

  it "displays bombs on a board" $ do
    let board = makeBoard 2 2 [(1, 1)]
        newBoard = openCell 1 1 $ openCell 1 2 board
        expected = unlines
          [
          "#####",
          "|*|.|",
          "-----",
          "| | |",
          "#####"
          ]
    format newBoard `shouldBe` expected

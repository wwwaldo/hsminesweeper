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
          "|.|.|",
          "-----",
          "|.|.|",
          "#####"
          ]
    format newBoard `shouldBe` expected

  it "displays bombs on a board" $ do
    let board = makeBoard 2 2 [(1, 1)]
        newBoard = openCell 1 1 $ openCell 1 2 board
        expected = unlines
          [
          "#####",
          "|*|1|",
          "-----",
          "| | |",
          "#####"
          ]
    format newBoard `shouldBe` expected

  it "Opens entire board when no bombs" $ do
    let board = makeBoard 3 3 []
        newBoard = openCell 1 1 board
        expected = unlines
          [
          "#######",
          "|.|.|.|",
          "-------",
          "|.|.|.|",
          "-------",
          "|.|.|.|",
          "#######"
          ]
    format newBoard `shouldBe` expected

  it "Adds a flag to a cell with a bomb" $ do
    let board = makeBoard 2 2 [(1, 1)]
        newBoard = addFlag 1 1 board
        expected = unlines
          [
          "#####",
          "|F| |",
          "-----",
          "| | |",
          "#####"
          ]
    format newBoard `shouldBe` expected

  it "Removes a flag on a cell without a bomb" $ do
    let board = makeBoard 2 2 []
        newBoard = removeFlag 1 1 (addFlag 1 1 board)
        expected = unlines
          [
          "#####",
          "| | |",
          "-----",
          "| | |",
          "#####"
          ]
    format newBoard `shouldBe` expected

  it "Removes a flag on a cell without a bomb" $ do
    let board = makeBoard 2 2 []
        newBoard = removeFlag 1 1 (addFlag 1 1 board)
        expected = unlines
          [
          "#####",
          "| | |",
          "-----",
          "| | |",
          "#####"
          ]
    format newBoard `shouldBe` expected

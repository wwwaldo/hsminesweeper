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


    -- `` turns prefix functions into infix

  it "shows board of different size" $ do
    let board = makeBoard 1 1
        expected = unlines
          [
          "###",
          "| |",
          "###"
          ]

    format board `shouldBe` expected

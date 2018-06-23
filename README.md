# hsminesweeper
Minesweeper in Haskell

Yay, Haskell! This is a console-based minesweeper, with cutting-edge ASCII character art and a blazing-fast UI.

(This is a joke, sorry)

![](hsminesweeper-sweet-ui.png)

# Building
This project uses the [stack](https://docs.haskellstack.org/en/stable/README/) toolchain for Haskell. Assuming you have this installed correctly, you can build from source by running the following commands from your favourite terminal:

1. `git clone`ing this repo;
1.1. `cd`ing to the project directory;
2. run `stack build` to compile;
3. run `stack exec runhaskell -- -isrc src/Main.hs` to run the program from the project directory. You should now see a lovingly crafted ASCII grid.

hsminesweeper uses 1-based (row, col) indexing.

# TODO
Add the ability to put flags on the grid.

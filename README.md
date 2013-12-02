Sudoku solving in haskell
==

The goal of this project was for me to learn haskell and so the code is very rough.

Usage:
> execute: ./Sudoku infile outfile

Infile is the filename of an file containing one or more sudokus.
The accepted length formats are squares of squares:
-	1^2^2 = 1
-	2^2^2 = 16
-	3^2^2 = 81
-	4^2^2 = 256
-	Higher should work but will be really slow

Values go from 1-9 to a-z with spaces, dots, commas, and zeros as empty cells
One sudoku per line for example:
> 1....8.74...3....98..6...5....267.1..7.....2..9.185....5...9..19....6...64.8....7
For a total 81 chars for a 9x9 sudoku.

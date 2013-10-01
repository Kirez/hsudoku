--WIP

--Imports
import Data.List

--Types
type Cell 		= ((Int, Int), [Int])
type Sudoku 	= [Cell] 
type CellSet 	= ([Cell], [Int])
type Row 		= (Int, CellSet)
type Column 	= (Int, CellSet)
type Block  	= ((Int,Int), CellSet)

--Function definitions
--gets
getSudokuSolution 	:: Sudoku 	-> Sudoku 	
getSudokuSolutions	:: Sudoku	-> [Sudoku] 
getSudokuRow 		:: Sudoku 	-> Int		-> Row
getSudokuColumn 	:: Sudoku 	-> Int		-> Column
getSudokuBlock 		:: Sudoku 	-> Int		-> Int		-> Block
getSudokuCell		:: Sudoku	-> Int		-> Int		-> Cell
getSudokuRows		:: Sudoku	-> [Row]
getSudokuColumns	:: Sudoku	-> [Column]
getSudokuBlocks		:: Sudoku	-> [Block]
getSudokuCells		:: Sudoku	-> [Cell]
getCellSetPossibles :: CellSet 	-> [Int]
getRowPossibles 	:: Row 		-> [Int]
getColumnPossibles 	:: Column 	-> [Int]
getBlockPossibles 	:: Block 	-> [Int]
getCellRowNum 		:: Cell 	-> Int
getCellColumnNum 	:: Cell 	-> Int
getCellBlockNum 	:: Cell 	-> Int
getRowCells 		:: Row 		-> [Cell]
getColumnCells		:: Column	-> [Cell]
getBlockCells		:: Block	-> [Cell]
getSudokuMissing	:: Sudoku	-> Int
getSudokuPlaced		:: Sudoku	-> Int
getRowMissing		:: Row		-> Int
getRowPlaced		:: Row		-> Int
getColumnMissing	:: Column	-> Int
getColumnPlaced		:: Column	-> Int
getBlockMissing		:: Block	-> Int
getBlockPlaced		:: Block	-> Int

--sets
setCell				:: Cell		-> Int		-> Cell
sudokuSetCell		:: Sudoku	-> Cell		-> Sudoku

--checks
rowIsValid			:: Row		-> Bool
columnIsValid		:: Column	-> Bool
blockIsValid		:: Block	-> Bool
cellISValid			:: Cell		-> Bool
sudokuIsValid		:: Sudoku 	-> Bool
sudokuIsDone		:: Sudoku	-> Bool

--Practical
makeEmptyCell		:: Int 		-> Int		-> Cell
sudokuFromList		:: [Int]	-> Sudoku




--Hazard zone
main = print "Nothing to see here"

--Psudocode
{-

getSolutions
	solveOne
		getPossibles
		setEnsured
		solveOne
-}

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
getSudokuSolution 	:: Sudoku 	-> Sudoku 	--Get a solution
getSudokuSolutions	:: Sudoku	-> [Sudoku] --Get all solutions

getSudokuRow 		:: Sudoku 	-> Int		-> Row
getSudokuColumn 	:: Sudoku 	-> Int		-> Column
getSudokuBlock 		:: Sudoku 	-> Int		-> Int		-> Block
getSudokuCell		:: Sudoku	-> Int		-> Int		-> Cell
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
sudokuIsValid		:: Sudoku 	-> Bool
sudokuIsDone		:: Sudoku	-> Bool

--Practical

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

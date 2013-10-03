--WIP

--Imports
import Data.List
import Data.Char
import System.IO

--Types
type Cell		= Int
type Sudoku		= [[Cell]]
type Row		= [Cell]
type Column		= [Cell]
type Block		= [Cell]
type Position 	= (Int, Int)

--Functions
{-solveSudoku		:: Sudoku	-> [Sudoku]-}

getCell			:: Sudoku	-> Position	-> Cell
getRow			:: Sudoku	-> Int		-> Row
getColumn		:: Sudoku	-> Int		-> Column
getBlock		:: Sudoku	-> Position	-> Block
getRowAt		:: Sudoku	-> Position	-> Row
getColumnAt		:: Sudoku	-> Position	-> Column
getBlockAt		:: Sudoku	-> Position	-> Block
getCells		:: Sudoku	-> [Cell]
getRows			:: Sudoku	-> [Row]
getColumns		:: Sudoku	-> [Column]
getBlocks		:: Sudoku	-> [Block]
getPossibles	:: [Cell]	-> [Cell]
getPossiblesAt	:: Sudoku	-> Position	-> [Cell]

isValid			:: [Cell]	-> Bool
isValidRow		:: Row		-> Bool
isValidColumn	:: Column	-> Bool 
isValidBlock	:: Block	-> Bool
isValidSudoku	:: Sudoku	-> Bool

readSudoku		:: String 	-> Sudoku
readSudokus		:: String 	-> [Sudoku]
showSudoku		:: Sudoku	-> String

--Practical

getCell s (x,y) = s !! y !! x

getRow s r 	= s !! r

getColumn s c = [getCell s (c, x) | x <- [0..8]]

getBlock s (x,y)
	| x >= 0 && x <= 2 && y >= 0 && y <= 2 = concat (map (grabBlockChunk s (x,y)) [0..2])
	| otherwise = []
	where
		grabBlockChunk s (x,y) n = take 3 (drop (x*3) (getRow s (y*3+n)))

getRowAt s (_,y) = getRow s y

getColumnAt s (x,_) = getColumn s x

getBlockAt s (x,y) = getBlock s (x `quot` 3, y `quot` 3)

getCells = concat

getRows s = map (getRow s) [0..8]

getColumns s = map (getColumn s) [0..8]

getBlocks s = [(getBlock s (x,y)) | y <- [0..2], x <- [0..2]]

getPossibles c = [x | x <- [1..9], not(x `elem` c)]

getPossiblesAt s (x,y) = [p | p <- [1..9], (p == (getCell s (x,y))) || (not(p `elem` nub(getRow s y ++ getColumn s x ++ getBlockAt s (x,y))) && (getCell s (x,y) == 0))]

isValid s = check (sort s)
	where
		check (x:xs)
			| xs == [] = True
			| x > 9 || x < 0 = False
			| x == (head xs) && x /= 0 = False
			| otherwise = check xs

isValidRow = isValid

isValidColumn = isValid

isValidBlock = isValid

isValidSudoku s = (all isValidRow (getRows s)) && (all isValidColumn (getColumns s)) && (all isValidBlock (getBlocks s))

--IO
		
showSudoku s = sudokuFormat (toString s)
	where
		sudokuFormat s
			| length s > 18 = take 9 s ++ "\n" ++ sudokuFormat (drop 9 s)
			| length s == 18 = take 9 s ++ "\n" ++ drop 9 s
			| length s == 9 = s
			| otherwise = []
		toString (r:rs)
			| r == [] = []
			| rs /= [] = map cellToChar r ++ toString rs
			| otherwise = map cellToChar r
		cellToChar n
			| n == 0 = '.'
			| n `elem` [1..9] = intToDigit n
			| otherwise = '.'
		
readSudoku s
	| length s /= 81 = [[]]
	| otherwise = (toRows.map fromChar) s
	where
		fromChar c
			| c == '*' = 0
			| c == '.' = 0
			| c == '_' = 0
			| c == '0' = 0
			| c `elem` ['1'..'9'] = read [c] :: Int
			| otherwise = 0
		toRows d
			| length d < 9 = [[]]
			| length d == 9 = [d]
			| otherwise = (take 9 d) : (toRows (drop 9 d))

readSudokus s = map readLine (lines s)
	where
		readLine l
			| l == [] = []
			| length l < 81 = []
			| otherwise = readSudoku (take 81 l)

testSudokus = 	"200040005506100000001002080000001200000000063304000050030007840002604000000090002\n\
				\980000006000030080030002900020097000300000004000450060008300050060070000100000048\n\
				\081600090000000000004037600600400500030000070007002004005210300000000000070004810\n\
				\000200041000070006002005080300000008040003600006050070000080010908004200170500000\n\
				\000190000010000090000030801060004930005080100094500080903040000020000070000059000\n\
				\007004060000600708000370040004000085030000090280000400090015000108002000040700800\n\
				\700020040052070600900005000000080010008106300010090000000200007006010820020060009"

main = do
	x <- readFile "sudokus.txt"
	let sudokus = readSudokus x
	let errors = [showSudoku (sudokus !! x) ++ "\n" | x <- [0..(length sudokus)-1],(isValidSudoku (sudokus !! x)) == False]
	let testSudoku = sudokus !! 0
	putStrLn (show (length errors))

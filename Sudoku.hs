import Data.List

type Sudoku = [Int]



sudokuSetIsWithinRange :: [Int] -> Bool
sudokuSetIsWithinRange st =
  wrong st + length st == 9
  where 
    wrong s = (length . filter (>9)) st + (length . filter (<0)) st
    
sudokuSetHasDoubles :: [Int] -> Bool
sudokuSetHasDoubles st
  | length (nub st) == length st = False
  | otherwise = True


sudokuSetIsValid :: [Int] -> Bool
sudokuSetIsValid st =
  if sudokuSetIsWithinRange st then
    if sudokuSetHasDoubles st then 
      False
    else
      True
  else
    False
    
sudokuSetContainsAll :: [Int] -> Bool
sudokuSetContainsAll st = sort st == [1..9]



sudokuSetIsDone ::  [Int] -> Bool
sudokuSetIsDone st =
  if sudokuSetIsValid st then 
    if sudokuSetContainsAll st then 
      True
    else
      False
  else
    False


getSudokuRow :: Sudoku -> Int -> [Int]
getSudokuRow s r = map ((!!) s) [r*9-9 .. r*9-1]

getSudokuColumn :: Sudoku -> Int -> [Int]
getSudokuColumn s c = map ((!!) s) [c-1 .. 9*8 + c -1, 9])

getSudokuBlock :: Sudoku -> Int -> [Int]
getSudokuBlock s b = map ((!! s)) []

sudokuRowsValid :: Sudoku -> Bool
sudokuRowsValid su
  | length $ nub (map validRow su [1..9]) < 2 = True
  | otherwise = False
  where 
    validRow s r = (sudokuSetIsValid . getSudokuRow s r)


sudokuColumnsValid :: Sudoku -> Bool
sudokuColumnsValid su
  | length $ nub (map validColumn su [1..9]) < 2 = True
  | otherwise = False
  where 
    validCol s r = (sudokuSetIsValid . getSudokuColumn s r)

sudokuIsValid :: Sudoku -> Bool
sudokuIsValid su =
  if sudokuRowsValid su then
    if sudokuColumnsValid su then
      if sudokuBlocksValid su then
        True
      else False
    else False
  else False

{-



sudokuBlocksValid :: Sudoku -> Bool
    





getSudokuColumn :: Sudoku -> Int -> [Int]

getSudokuBlock :: Sudoku -> Int -> [Int]

solveSudoku :: Sudoku -> Sudoku

sudokuIsSolved :: Sudoku -> Bool

canSolveSudoku :: Sudoku -> Bool

-}



main = do
  let testSudoku = [
                    0,0,0,2,6,0,7,0,1,
                    6,8,0,0,7,0,0,9,0,
                    1,9,0,0,0,4,5,0,0,
                    8,2,0,1,0,0,0,4,0,
                    0,0,4,6,0,2,9,0,0,
                    0,5,0,0,0,3,0,2,8,
                    0,0,9,3,0,0,0,7,4,
                    0,4,0,0,5,0,0,3,6,
                    7,0,3,0,1,8,0,0,0
                    ]
  let solution = [
                  4,3,5,2,6,9,7,8,1,
                  6,8,2,5,7,1,4,9,3,
                  1,9,7,8,3,4,5,6,2,
                  8,2,6,1,9,5,3,4,7,
                  3,7,4,6,8,2,9,1,5,
                  9,5,1,7,4,3,6,2,8,
                  5,1,9,3,2,6,8,7,4,
                  2,4,8,9,5,7,1,3,6,
                  7,6,3,4,1,8,2,5,9
                 ]
  putStr (show (getSudokuRow [0..80] 2))



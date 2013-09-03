import Data.List

type Sudoku = [Int]

sudokuSetIsWithinRange :: [Int] -> Bool
sudokuSetIsWithinRange st =
  wrong st + length st == 9
  where 
    wrong s = (length . filter (>9)) st + (length . filter (<0)) st
    
sudokuSetHasDoubles :: [Int] -> Bool
sudokuSetHasDoubles st
  | (length (nub (filter (/=0) st))) == (length (filter (/=0) st)) = False
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
getSudokuRow s r
  | (length s == 81) && (r > 0) && (r < 10) = (map ((!!) s) [r*9-9 .. r*9-1])
  | otherwise = [] 
  

getSudokuColumn :: Sudoku -> Int -> [Int]
getSudokuColumn s c
  | (length s == 81) && (c > 0) && (c < 10) = (map ((!!) s) [c-1,c+8 .. 9*8 + c -1])
  | otherwise = []

getSudokuBlock :: Sudoku -> Int -> [Int]
getSudokuBlock s b
  | (length s == 81) && (b > 0) && (b < 10) = getBlock s b
  | otherwise = []
  where
    getBlock s b
      | b == 1 = (dropTake 0 3 (getSudokuRow s 1)) ++ (dropTake 0 3 (getSudokuRow s 2)) ++ (dropTake 0 3 (getSudokuRow s 3))
      | b == 2 = (dropTake 3 3 (getSudokuRow s 1)) ++ (dropTake 3 3 (getSudokuRow s 2)) ++ (dropTake 3 3 (getSudokuRow s 3))
      | b == 3 = (dropTake 6 3 (getSudokuRow s 1)) ++ (dropTake 6 3 (getSudokuRow s 2)) ++ (dropTake 6 3 (getSudokuRow s 3))
      
      | b == 4 = (dropTake 0 3 (getSudokuRow s 4)) ++ (dropTake 0 3 (getSudokuRow s 5)) ++ (dropTake 0 3 (getSudokuRow s 6))
      | b == 5 = (dropTake 3 3 (getSudokuRow s 4)) ++ (dropTake 3 3 (getSudokuRow s 5)) ++ (dropTake 3 3 (getSudokuRow s 6))
      | b == 6 = (dropTake 6 3 (getSudokuRow s 4)) ++ (dropTake 6 3 (getSudokuRow s 5)) ++ (dropTake 6 3 (getSudokuRow s 6))
      
      | b == 7 = (dropTake 0 3 (getSudokuRow s 7)) ++ (dropTake 0 3 (getSudokuRow s 8)) ++ (dropTake 0 3 (getSudokuRow s 9))
      | b == 8 = (dropTake 3 3 (getSudokuRow s 7)) ++ (dropTake 3 3 (getSudokuRow s 8)) ++ (dropTake 3 3 (getSudokuRow s 9))
      | b == 9 = (dropTake 6 3 (getSudokuRow s 7)) ++ (dropTake 6 3 (getSudokuRow s 8)) ++ (dropTake 6 3 (getSudokuRow s 9)) 
      where
        dropTake d t a = ((take t) . (drop d)) a


-- 3 All valid [True*9] which after nub returns [True] of length 1
sudokuRowsValid :: Sudoku -> Bool
sudokuRowsValid su
  | (nub (map (validRow su) [1..9])) == [True] = True
  | otherwise = False
  where 
    validRow s r = (sudokuSetIsValid . getSudokuRow s) r

{-

-- 3 All valid [True*9] which after nub returns [True] of length 1
sudokuColumnsValid :: Sudoku -> Bool
sudokuColumnsValid su
  | length $ nub (map validColumn su [1..9]) < 2 = True
  | otherwise = False
  where 
    validColumn s r = (sudokuSetIsValid . getSudokuColumn s r)

-- 3 All valid [True*9] which after nub returns [True] of length 1    
sudokuBlocksValid :: Sudoku -> Bool
sudokuBlocksValid su
  | (length $ (nub (map validBlock su [1..9]))) < 2 = True
  | otherwise = False
  where
    validBlock s r = (sudokuSetIsValid . (getSudokuBlock s r)) -- validBlock :: Sudoku -> Int -> [Int] -> Bool  #(Hell Yea!)


sudokuIsValid :: Sudoku -> Bool
sudokuIsValid su =
  if sudokuRowsValid su then
    if sudokuColumnsValid su then
      if sudokuBlocksValid su then
        True
      else False
    else False
  else False
  
-}

{-



sudokuBlocksValid :: Sudoku -> Bool
    





getSudokuColumn :: Sudoku -> Int -> [Int]

getSudokuBlock :: Sudoku -> Int -> [Int]

solveSudoku :: Sudoku -> Sudoku

sudokuIsSolved :: Sudoku -> Bool

canSolveSudoku :: Sudoku -> Bool

-}


testSudoku =  [
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
solution =  [
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

main = print (sudokuRowsValid testSudoku)



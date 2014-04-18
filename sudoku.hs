import Data.List
import Data.Char
import Data.Maybe
import Control.Monad
import System.IO
import System.Environment

type Sudoku = (Grid, [Symbol], [Symbol], Int) --Grid, Symbols, SymbolsLeft, Size
type Grid = [String]
type Row = String
type Column = String
type Block = String
type Values = String
type Position = Int
type Symbol = Char
type Point = (Position, Position)
type Possible = (Point, [Symbol])
type PossibleGrid = (Grid, [Possible])

data Set = Row | Column | Block

gridSize :: Grid -> Int

sValues = ['a'..'z'] ++ ['1'..'9'] 	--symbol value
sZeros = "0 _.,*-"					--symbol zero
aValues = (sValues ++ sZeros)		--acceptable values

blockSize :: Grid -> Int
numRows :: Grid -> Int
numColumns :: Grid -> Int
numBlocks :: Grid -> Int
position :: Grid -> Point -> Position
point :: Grid -> Position -> Point
chunk :: Int -> [a] -> [[a]]
valid :: Grid -> Bool				
symbols :: Grid -> [Symbol]
symbolsLeft :: Grid -> [Symbol]-> [Symbol]
symbolsDone :: Grid -> [Symbol]
printFormat :: Grid -> String
readGrid :: String -> Grid
showGrid :: Grid -> String
cell :: Grid -> Position -> Symbol
cellAt :: Grid -> Point -> Symbol
cells :: Grid -> [Symbol]
row :: Grid -> Position -> Row
rowAt :: Grid -> Point -> Row
rows :: Grid -> [Row]
column :: Grid -> Position -> Column
columnAt :: Grid -> Point -> Column
columns :: Grid -> [Column]

block :: Grid -> Position -> Block
blockAtCP :: Grid -> Point -> Block --At a cells point
blockAtBP :: Grid -> Point -> Block --At a blockwise point
possible :: Sudoku -> Point -> Possible
possibles :: Sudoku -> [Possible]
eliminate :: Sudoku -> Sudoku
set :: Grid -> Point -> Symbol -> Grid
complete :: Grid -> Bool
choices :: Sudoku -> Int
search :: [Sudoku] -> Maybe Sudoku
--search' :: [Possible] -> Maybe Grid
bestChoice :: [Possible] -> Possible
possibleSudokus :: Sudoku -> [Sudoku]
sortPossibles :: [Possible] -> [Possible]
solve :: Sudoku -> Sudoku
grid :: Sudoku -> Grid
sudoku :: Grid -> Sudoku

--solvable :: Grid -> Bool

--Extra

gridSize (fr:rs) = length fr
gridSize [] = 0
blockSize g = round ( sqrt ( fromIntegral ( gridSize g )))
numRows = gridSize
numColumns = gridSize
numBlocks = gridSize
position g (x,y) = y * (numColumns g) + x
grid (g,_,_,_) = g
sudoku g = (g, sbs, sbslft, gs)
	where
		sbs = symbols g
		sbslft = symbolsLeft g sbs
		gs = gridSize g

point g p = pt
	where
		pt = (x,y)
		x = p - y * (gridSize g)
		y = p `quot` (gridSize g)

readGrid sg = chunk (round (sqrt ( fromIntegral (length fsg)))) [c | c <- fsg, c `elem` aValues]
	where
		fsg = filter (`elem` aValues) sg

showGrid = concat

chunk _ [] = []
chunk n a = cnk : (chunk n rst)
	where 
		(cnk , rst) = (splitAt n a)
		
valid g = if v then True else False
	where
		ts = length (concat g)
		ss = length (symbols g)
		gs = gridSize g
		bs = blockSize g
		rc = numRows g
		cc = numColumns g
		v = ((gs*gs) == ts && (bs*bs) == gs && rc == gs && cc == rc && ss == rc && rc*cc == ts) --Maybe a bit exagerated.. but you know..

symbols g = sort (filter (`elem` sValues) (nub (concat g)))

symbolsLeft g sbs = [cs | cs <- sbs, (length(filter (==cs) (concat g))) /= (gridSize g)]

symbolsDone g = [cs | cs <- (symbols g), (length(filter (==cs) (concat g))) == (gridSize g)]

printFormat g = fg
	where
		gs = gridSize g
		bs = blockSize g
		rs = (gs * 2 + bs + 1)
		sl = "+" ++ (replicate (gs * 2 + bs*2 - 1) '-') ++ "+"
		ln = (concat (intersperse "+" ([(replicate (bs*2) '-')] ++ (replicate (bs-2) (replicate (bs*2+1) '-')) ++ [(replicate (bs*2) '-')])))
		fg = addLines (map (++"\n") (map (intersperse ' ') (map concat (map (intersperse "|") (map (chunk bs) g)))))
		addLines d = sl ++ "\n" ++ concat["| " ++ l ++ " |\n" | l <- lines(concat(intercalate [ln++"\n"] (chunk bs d)))] ++ sl ++ "\n"

cellAt g (x,y) = g !! y !! x

cell g p = cellAt g (point g p)

cells = concat
		
row g p = g !! p
	
rowAt g (_,y) = row g y

rows g = map (row g) [0..(numRows g)-1]

column g p = [r !! p | r <- rows g]
		
columnAt g (x,_) = column g x

columns g = map (column g) [0..(numColumns g)-1]

block g p = blockAtBP g (x,y)
	where
		y = p `quot` bs
		x = p - y * bs
		bs = blockSize g
		
blockAtBP g (x,y) = blk
	where
		bs = blockSize g
		rs = take bs (drop (y*bs) (rows g))
		cks = map (chunk bs) rs
		blk = concat(concatMap (take 1) (map (drop x) cks))
		
blockAtCP g (x,y) = blockAtBP g (nx,ny)
	where
		bs = blockSize g
		nx = x `quot` bs
		ny = y `quot` bs

set g (x,y) c
	| x > gs || y > gs = g
	| otherwise = ng
	where
		gs = gridSize g
		ng = [[if ix == x && iy == y then c else cellAt g (ix,iy) | ix <- [0..gs-1]] | iy <- [0..gs-1]]

eliminate s@(g,sbs,sbslft,gs) = ng
	where
		elims = [(x,y,e) | x <- [0..gs-1], y <- [0..gs-1], let p = snd (possible s (x,y)), length p == 1, e <- p]
		ng = elim g elims 
		elim g [] = s
		elim g ((x,y,e):_) = eliminate ((set g (x,y) e),sbs,sbslft,gs)
		
complete g = valid g && (length $ symbolsLeft g (symbols g)) == 0

possible s@(g,sbs,sbslft,_) p@(x,y) 
	| ce `elem` sbs = (p,[])
	| otherwise = (p,psb)
	where
		ce = cellAt g (x,y)
		r = row g y
		c = column g x
		b = blockAtCP g (x,y)
		sl = sbslft
		np = nub (r++c++b)
		psb = [p | p <- sl, not (p `elem` np)]
		
possibles s@(g,_,_,_) = filter ((>0) . length . snd) (map (possible s) (map (point g) gridRange))
	where
		gs = gridSize g
		gridRange = [0..gs*gs-1]

choices g = length (concatMap snd (possibles g))
choices' g = length (filter (`elem` sValues) (concat g))

search (s@(g,sbs,sbslft,gs):sdslft)
	| g == [] = Nothing
	| complete g = Just s
	| length psbsds == 0 = search sdslft
	| valid g && rst /= [] = search(bst ++ sdslft ++ rst)
	| valid g = search (bst ++ sdslft)
	| otherwise = search sdslft
	where
		psbsds = map eliminate (possibleSudokus s)
		bst = if length psbsds > 0 then [head psbsds] else []
		rst = if length psbsds > 0 then tail psbsds else []

sortPossibles = sortBy comparePossibles
	where
		comparePossibles a b = ((length . snd) a) `compare` ((length . snd) b)
		
bestChoice p = head (sortPossibles p)
		
possibleSudokus s@(g,_,_,_) = gds
	where
		psbs = sortPossibles (possibles s)
		gds = map sudoku (concatMap (setPossible g) psbs)
		setPossible g (p,s) = gs
			where
				gs = map (set g p) s
				
solve s@(g,_,_,_)
	| complete g = s
	| valid g /= True = s
	| otherwise = case solution of
						Just _ -> fromJust solution
						Nothing -> s
	where
		solution = (search [s])
		
main = do
	args <- getArgs
	let sudokus = map sudoku (map readGrid (args))
	mapM putStrLn (map showGrid (map grid sudokus))
	mapM putStrLn (map printFormat (map (grid.solve) sudokus))
	putStrLn ("Done solving " ++ (show $ length sudokus) ++ " sudokus!")

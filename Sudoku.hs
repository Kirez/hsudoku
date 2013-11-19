import Data.List
import Data.Char

type Grid = [String]
type Row = String
type Column = String
type Block = String
type Values = String
type Position = Int
type Symbol = Char
type Point = (Position, Position)

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
symbolsLeft :: Grid -> [Symbol]
symbolsDone :: Grid -> [Symbol]
printFormat :: Grid -> String
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
psbSymbolsAt :: Grid -> Point -> [Symbol]
eliminate :: Grid -> Grid
set :: Grid -> Point -> Symbol -> Grid
assign :: Grid -> Point -> Symbol -> Grid
complete :: Grid -> Bool

--solvable :: Grid -> Bool

--Extra

gridSize (fr:rs) = length fr
blockSize g = round ( sqrt ( fromIntegral ( gridSize g )))
numRows = gridSize
numColumns = gridSize
numBlocks = gridSize
position g (x,y) = y * (numColumns g) + x
point g p = pt
	where
		pt = (x,y)
		x = p - y * (gridSize g)
		y = p `quot` (gridSize g)

readGrid :: String -> Grid
readGrid sg = chunk (round (sqrt ( fromIntegral (length fsg)))) [c | c <- fsg, c `elem` aValues]
	where
		fsg = filter (`elem` aValues) sg


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

symbolsLeft g = [cs | cs <- (symbols g), (length(filter (==cs) (concat g))) /= (gridSize g)]

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

cellAt g (x,y)
	| length g <= y = '0'
	| length (g !! y) <= x = '0'
	| otherwise = g !! y !! x

cell g p = cellAt g (point g p)

cells = concat
		
row g p
	| length g <= p = []
	| otherwise = g !! p
	
rowAt g (_,y) = row g y

rows g = map (row g) [0..(numRows g)-1]

column g p = [r !! p | r <- rows]
	where
		rows = filter (\x -> length x > p) [row g r | r <- [0..(length g) - 1]]
		
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
		
psbSymbolsAt g (x,y)
	| ce `elem` s = []
	| otherwise = psb
	where
		s = symbols g
		ce = cellAt g (x,y)
		r = row g y
		c = column g x
		b = blockAtCP g (x,y)
		sl = symbolsLeft g
		np = nub (r++c++b)
		psb = [p | p <- sl, not (p `elem` np)]

set g (x,y) c
	| x > gs || y > gs = g
	| otherwise = ng
	where
		gs = gridSize g
		ng = [[if ix == x && iy == y then c else cellAt g (ix,iy) | ix <- [0..gs-1]] | iy <- [0..gs-1]]
		
assign g p = eliminate . (set g p)

eliminate g = ng
	where
		gs = gridSize g
		elims = [(x,y,e) | x <- [0..gs-1], y <- [0..gs-1], let p = psbSymbolsAt g (x,y), length p == 1, e <- p]
		ng = elim g elims
		elim g [] = g
		elim g ((x,y,e):_) = eliminate (set g (x,y) e)
		
complete g = valid g && (length $ symbolsLeft g) == 0

		
main = do
	let g5 = "64..139..1...264...29.457....2...83.86..37.197..2.9.....13..69.9364.8.2...5......"
	let test = readGrid g5
	putStrLn (printFormat test)
	let el = eliminate test
	putStrLn (printFormat el)

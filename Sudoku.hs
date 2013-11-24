import Data.List
import Data.Char
import Data.Maybe

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
possible :: Grid -> Point -> Possible
possibles :: Grid -> [Possible]
eliminate :: Grid -> Grid
set :: Grid -> Point -> Symbol -> Grid
assign :: Grid -> Point -> Symbol -> Grid
complete :: Grid -> Bool
choices :: Grid -> Int
search :: [Grid] -> Maybe Grid
--search' :: [Possible] -> Maybe Grid
bestChoice :: [Possible] -> Possible
possibleGrids :: Grid -> [Grid]
sortPossibles :: [Possible] -> [Possible]
solve :: Grid -> Grid
orderGrids :: [Grid] -> [Grid]

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
		elims = [(x,y,e) | x <- [0..gs-1], y <- [0..gs-1], let p = snd $ possible g (x,y), length p == 1, e <- p]
		ng = elim g elims
		elim g [] = g
		elim g ((x,y,e):_) = eliminate (set g (x,y) e)
		
complete g = valid g && (length $ symbolsLeft g) == 0

possible g p@(x,y)
	| ce `elem` s = (p,[])
	| otherwise = (p,psb)
	where
		s = symbols g
		ce = cellAt g (x,y)
		r = row g y
		c = column g x
		b = blockAtCP g (x,y)
		sl = symbolsLeft g
		np = nub (r++c++b)
		psb = [p | p <- sl, not (p `elem` np)]
		
possibles g = filter ((>0) . length . snd) (map (possible g) (map (point g) gridRange))
	where
		gs = gridSize g
		gridRange = [0..gs*gs-1]

choices g = length (concatMap snd (possibles g))
choices' g = length (filter (`elem` sValues) (concat g))

search (g:gs)
	| g == [] = Nothing
	| complete g = Just g
	| valid g = search(map eliminate (possibleGrids g) ++ gs)
	| otherwise = search gs

sortPossibles = sortBy comparePossibles
	where
		comparePossibles a b = ((length . snd) a) `compare` ((length . snd) b)
		
bestChoice p = head (sortPossibles p)
		
possibleGrids g = gds
	where
		psbs = sortPossibles (possibles g)
		gds = concatMap (setPossible g) psbs
		setPossible g (p,s) = gs
			where
				gs = map (set g p) s
				
solve g
	| complete g = g
	| valid g /= True = g
	| otherwise = case solution of
						Just _ -> fromJust solution
						Nothing -> g
	where
		solution = (search [g])
		
orderGrids = sortBy compareGrids
	where compareGrids a b = choices a `compare` choices b
		
main = do
	let g5 = "020030090000907000900208005004806500607000208003102900800605007000309000030020050"
	let test1 = readGrid g5
	putStrLn (printFormat $ solve test1)

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
positionAt :: Point -> Grid -> Position
pointAt :: Position -> Grid -> Point
chunk :: Int -> [a] -> [[a]]
valid :: Grid -> Bool				
symbols :: Grid -> [Symbol]
symbolsLeft :: Grid -> [Symbol]
symbolsDone :: Grid -> [Symbol]
printFormat :: Grid -> String
--possibleLeft :: Grid -> [Symbol]
--solvable :: Grid -> Bool

gridSize (fr:rs) = length fr
blockSize g = round ( sqrt ( fromIntegral ( gridSize g )))
numRows = gridSize
numColumns = gridSize
numBlocks = gridSize
positionAt (x,y) g = y * (numColumns g) + x
pointAt p g = pt
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
		addLines d = sl ++ "\n" ++ concat["| "++l ++ " |\n" | l <- lines(concat(intercalate [ln++"\n"] (chunk bs d)))] ++ sl ++ "\n"

main = do
	let g4 = "1234000000000000"
	let g3 = "123456789abcdefghijklmnop000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
	let g2 = "123456789abcdefg000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
	let g = "200040005506100000001002080000001200000000063304000050030007840002604000000090002"
	let test = readGrid g
	let test2 = readGrid g2
	let test3 = readGrid g3
	let test4 = readGrid g4
	mapM_ putStrLn test
	mapM_ putStrLn test2
	putStrLn (printFormat test3)
	--mapM_ putStrLn (map show (map concat (map (intersperse "|") (map (chunk (blockSize test)) test))))

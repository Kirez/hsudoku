type Position 	= (Int, Int)
type Cell 		= (Position, Int)
type Set		= [Cell]
type Row		= (Int, Set)
type Column		= (Int, Set)
type Block		= (Int, Set)
type Grid		= (Int, Set)

getCell :: Grid -> Position -> Cell
getRow	:: Grid -> Int -> Row
getColumn :: Grid -> Int -> Column
getBlock :: Grid -> Int -> Block

rowAt :: Grid -> Position -> Row
columnAt :: Grid -> Position -> Column
blockAt :: Grid -> Position -> Block

getColumns :: Grid -> [Column]
getRows		:: Grid -> [Row]
getBlocks :: Grid -> [Block]

getCell g (x, y) = (snd g) !! y !! x
getRow g p = (snd g) !! p
getBlock g p = blocklen 
	where
		blockSize = round (sqrt (head g))

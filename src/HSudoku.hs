module HSudoku where

import Data.List
import Data.Ord (comparing)
import Data.List.Split (chunksOf)
import Data.Array.IArray
import Data.Either (rights)
import Data.Maybe (isJust, fromJust, isNothing)
import Data.Function (on)
import qualified Data.Set as S

import Control.Monad (foldM, replicateM)

data Letter = A
            | B
            | C
            | D
            | E
            | F
            | G
            | H
            | I deriving (Show, Read, Eq, Ord, Enum, Ix)

data Digit  = One
            | Two
            | Three
            | Four
            | Five
            | Six
            | Seven
            | Eight
            | Nine deriving (Eq, Ord, Enum, Ix)

type Coord  = (Letter, Digit)
type Unit   = [Coord]
type Cell   = Either [Digit] Digit
type Grid   = Array Coord Cell

type Technique = Grid -> Maybe Grid

instance Show Digit where
    show One    = "1"
    show Two    = "2"
    show Three  = "3"
    show Four   = "4"
    show Five   = "5"
    show Six    = "6"
    show Seven  = "7"
    show Eight  = "8"
    show Nine   = "9"

digits :: [Digit]
digits = [One .. Nine]

letters :: [Letter]
letters = [A .. I]

coords :: [Coord]
coords = cross letters digits

box :: (Coord, Coord)
box = ((A, One), (I, Nine))

units :: Array Coord [Unit]
units = array box [(c, [cs | cs <- unitList, c `elem` cs]) | c <- coords]

peers :: Array Coord [Coord]
peers = array box [(c, filter (/=c) . S.toList . S.fromList . concat $ (units ! c)) | c <- coords]

unitList :: [Unit]
unitList =  [cross letters [c] | c <- digits] ++
            [cross [r] digits | r <- letters] ++
            [cross ls ds | ls <- chunksOf 3 letters, ds <- chunksOf 3 digits]

techniques :: [Technique]
techniques = [single, hiddenSingle, hiddenDouble, double]
--techniques = [single, hiddenSingle, hiddenDouble, hiddenTripple, double, tripple]
--techniques = [single, hiddenSingle, hiddenDouble, hiddenTripple, double, tripple]
--techniques = [single, hiddenSingle, hiddenDouble, hiddenTripple, double, tripple]
--techniques = [single, hiddenSingle, hiddenDouble, hiddenTripple, double, tripple]

emptyGrid :: Grid
emptyGrid = array box [(c, Left digits) | c <- coords]

--Logic

-- |Takes a grid and returns [(Unit, [[Coord]])] where each
--  (Unit, [[Coord]]) represents a unit and its kindred sets where
--  each list of Coord represents the locations of the kindred cells
--  Whether or not a cell is kindred with another is determined by the Int.
--  For cells to belong in a kindred set they must contain x number of possible
--  digits and there must be exactly x number of kindreds in the unit
findKindredCells :: Grid -> Int -> [(Unit, [[Coord]])]
findKindredCells g n = kindreds
    where
        unsolvedList = map (filter (not . cellFilled . (g !))) unitList
        nsList = map (filter ((==n) . length . (\(Left x) -> x) . (g !)))
                    unsolvedList
        eqList = map (filter ((==n) . length) . groupBy ((==) `on` (!) g))
                    nsList
        kindreds = filter (not . null . snd) (zip unitList eqList)

        --eqList =    groupBy ((\(Left a) (Left b) -> a == b) . (g !)) nsList

--findSingles :: Grid -> [([Coord], [Digit])]

single :: Technique
single g = ng
    where
        oneks = findKindredCells g 1
        assignments = S.toList . S.fromList $
            [ (coord, head digits) | (_, ones) <- oneks
            , one <- ones, coord <- one
            , let digits = (\(Left x) -> x) (g ! coord)
            ]
        ng = if not . null $ assignments
             then foldM assign g (S.toList . S.fromList $ assignments)
             else Nothing

hiddenSingle :: Technique
hiddenSingle g = ng
    where
        ums = zip unitList (map (missing g) unitList)
        hss = S.toList . S.fromList $
            [ (coord, digit) | (unit, ms) <- ums, digit <- ms
            , let coords = possibles g unit [digit]
            , let coord = head coords, length coords == 1
            ]
        ng = if not . null $ hss then foldM assign g hss else Nothing

hiddenDouble :: Technique
hiddenDouble g = ng
    where
        uhd u = S.toList . S.fromList $
            [ hd | a <- twops, b <- twops \\ [a]
            , let hd = (sort [fst a, fst b], snd a)
            , snd a == snd b
            ]
            where
                ms = missing g u
                twops = [ (m, coords) | m <- ms
                        , let coords = possibles g u [m]
                        , length coords == 2
                        ]

        uhds = filter ((>0) . length . snd) $ zip unitList (map uhd unitList)
        toElim =
            [ (c, d) | (u, uhd) <- uhds, c <- u, (hds, hcs) <- uhd
            , let psb = (\a ->  case a of
                                    Left x -> x
                                    Right _ -> []) (g ! c)
            , let ds = if c `elem` hcs
                       then psb \\ hds
                       else filter (`elem` hds) psb
            , d <- ds
            , case g ! c of
                Left _  -> True
                Right _ -> False
            ]
        ng = if not . null $ toElim then foldM eliminate g toElim else Nothing

hiddenTripple :: Technique
hiddenTripple g = ng
    where
        uht u = S.toList . S.fromList $
            [ ht | a <- trips
            , b <- trips \\ [a]
            , c <- trips \\ [a, b]
            , let ht = (sort [fst a, fst b, fst c], snd a)
            , snd a == snd b, snd a == snd c
            ]
            where
                ms = missing g u
                trips = [ (m, coords) | m <- ms
                        , let coords = possibles g u [m]
                        , length coords == 3
                        ]

        uhts = filter ((>0) . length . snd) $ zip unitList (map uht unitList)
        toElim =
            [ (c, d) | (u, uht) <- uhts, c <- u, (hts, hcs) <- uht
            , let psb = (\a -> case a of
                                    Left x -> x
                                    Right _ -> []) (g ! c)
            , let ds = if c `elem` hcs
                       then psb \\ hts
                       else filter (`elem` hts) psb
            , d <- ds
            , case g ! c of
                Left _  -> True
                Right _ -> False
            ]
        ng = if not . null $ toElim then foldM eliminate g toElim else Nothing

double :: Technique
double g = ng
    where
        twoks = findKindredCells g 2
        toElim = S.toList . S.fromList $
            [ (c, ds) | (u, twos) <- twoks
            , two <- twos, c <- u \\ two
            , ds <- (\(Left x) -> x) (g ! head two)
            ]
        ng  = if not . null $ toElim then foldM eliminate g toElim else Nothing

tripple :: Technique
tripple g = ng
    where
        tripks = findKindredCells g 3
        toElim = S.toList . S.fromList $
            [ (c, ds) | (u, trips) <- tripks
            , trip <- trips, c <- u \\ trip
            , ds <- (\(Left x) -> x) (g ! head trip)
            ]
        ng = if not . null $ toElim then foldM eliminate g toElim else Nothing

applyTechnique :: Technique -> Grid -> Maybe Grid
applyTechnique t g
    | isNothing ng = Nothing
    | fromJust ng == g = Nothing
    | otherwise = ng
    where
        ng = t g

applyTechniques :: [Technique] -> Grid -> Maybe Grid
applyTechniques (t:ts) g = case ng of
    Nothing -> applyTechniques ts g
    Just _  -> ng
    where
        ng = applyTechnique t g
applyTechniques _ _ = Nothing

eliminate :: Grid -> (Coord, Digit) -> Maybe Grid
eliminate g (c, d) = case currentCell of
    Right _ -> Just g
    Left ds -> let newDigits = delete d ds in
        if null newDigits
        then Nothing
        else Just (g // [(c, Left newDigits)])
    where
        currentCell = g ! c

assign :: Grid -> (Coord, Digit) -> Maybe Grid
assign g (c, d) = case oldCell of
    Right _ -> Just g
    Left ds -> foldM eliminate ng (zip cellPeers (repeat d))
    where
        oldCell = g ! c
        newCell = (c, Right d)
        cellPeers = peers ! c
        ng = g // [newCell]


unsolvedByCertainty :: Grid -> [(Coord, [Digit])]
unsolvedByCertainty g = sortBy (comparing (length . snd)) unsolvedAssocs
    where unsolvedAssocs = [(coord, psbs) | (coord, Left psbs) <- assocs g]

mostCertainUnsolved :: Grid -> Maybe (Coord, [Digit])
mostCertainUnsolved g = if null usbc then Nothing else Just (head usbc)
    where usbc = unsolvedByCertainty g

search :: Grid -> Maybe Grid
search g = if isJust mostC && length digits > 1 then
    case solve ng of
        Just x -> Just x
        Nothing -> solve (g // [(coord, if length ndigits > 1
            then Left ndigits
            else Right $ head ndigits)])
    else Nothing
    where
        mostC = mostCertainUnsolved g
        coord = fst . fromJust $ mostC
        digits = snd . fromJust $ mostC
        digit = head digits
        ndigits = delete digit digits
        ng = g // [(coord, Right digit)]

trySolve :: Grid -> Either Grid Grid
trySolve g = if isSolved result
    then Right result
    else Left result
    where
        result = maybeUntilNoDiff (applyTechniques techniques) g

solve :: Grid -> Maybe Grid
solve g = case result of
    Left x  -> search x
    Right x -> Just x
    where
        result = trySolve g

--Util

-- |Takes a grid and a list of coordinates to check against a list of digits
--  Returns a list of coordinates where the digits are possible
possibles :: Grid -> [Coord] -> [Digit] -> [Coord] -- TODO: Optimize
possibles g cs ds = ps
    where
        dict = [(coord, cell) | coord <- cs, let cell = g ! coord]
        ps = [coord | (coord, Left ds') <- dict, all (`elem` ds') ds]

missing :: Grid -> Unit -> [Digit]
missing g u = digits \\ rs
    where
        rs = rights (map (g!) u)

maybeUntilNoDiff :: Eq a => (a -> Maybe a) -> a -> a
maybeUntilNoDiff f a = case na of
    Nothing -> a
    Just x  -> if x == a then a else maybeUntilNoDiff f x
    where
        na = f a

untilNoDiff :: Eq a => (a -> a) -> a -> a
untilNoDiff f a = if a == na then a else untilNoDiff f na where na = f a

cross :: [l] -> [d] -> [(l,d)]
cross ls ds  = [(l,d) | l <- ls, d <- ds]

cellFilled :: Cell -> Bool
cellFilled c = case c of
    Right _ -> True
    Left _  -> False

isSolved :: Grid -> Bool
isSolved g = all (==[One .. Nine]) $ map (sort.rights) [map (g !) u | u <- unitList]

--Parse

parseGrid :: String -> Maybe Grid
parseGrid s = foldM assign emptyGrid assignments
    where
        czip = zip coords s
        vals = filter ((`elem` ['1' .. '9']) . snd) czip
        assignments = [ (c, d) | (c, dc) <- vals, let d = readDigit dc]

--Read / Show

showCoord :: Coord -> String
showCoord (l, d) = show l ++ show d

readDigit :: Char -> Digit
readDigit c
    | c == '1' = One
    | c == '2' = Two
    | c == '3' = Three
    | c == '4' = Four
    | c == '5' = Five
    | c == '6' = Six
    | c == '7' = Seven
    | c == '8' = Eight
    | c == '9' = Nine
    | otherwise = error $ "Invalid Char: " ++ [c]

showCell :: Cell -> String
showCell (Right d)  = show d
showCell (Left _)  = "."

showGrid :: Grid -> String
showGrid = concatMap showCell . elems

formatGridString :: String -> String
formatGridString s = unlines $ [upperline] ++ vprawls ++ [lowerline]
    where
        upperline   = "┏━━━━━━━━━━━┳━━━━━━━━━━━┳━━━━━━━━━━━┓"
        innerline   = "┣━━━━━━━━━━━╋━━━━━━━━━━━╋━━━━━━━━━━━┫"
        lowerline   = "┗━━━━━━━━━━━┻━━━━━━━━━━━┻━━━━━━━━━━━┛"
        boxline     = "┃───┼───┼───┃───┼───┼───┃───┼───┼───┃"
        rawls = chunksOf 9 s
        prawls =    map (intersperse ' ' . (\x -> "┃" ++ x ++ "┃") .
                    intercalate "┃" . map (intersperse '│') . chunksOf 3) rawls
        vprawls =   intercalate [innerline] . chunksOf 5 .
                    concatMap (intersperse boxline) . chunksOf 3 $ prawls

--IO

printSideBySideGridStrings :: (String, String) -> IO ()
printSideBySideGridStrings (s1, s2) = putStrLn $ lmerge (s1, s2)
    where
        lmerge (a, b) = unlines [a ++ " " ++ b |
                                (a, b) <- zip
                                (lines $ formatGridString s1)
                                (lines $ formatGridString s2)]

printSideBySideGrids :: (Grid, Grid) -> IO ()
printSideBySideGrids (a, b) = printSideBySideGridStrings
    (showGrid a, showGrid b)

printGrid :: Grid -> IO ()
printGrid = putStrLn . formatGridString . showGrid

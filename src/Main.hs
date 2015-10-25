module Main where

import HSudoku
import Data.Maybe (fromJust, isJust, mapMaybe)

main :: IO ()
main = do
    content <- getContents
    let grids = mapMaybe parseGrid (lines content)
    let results = zip grids (map solve grids)
    mapM_ (putStrLn . \g -> maybe (showGrid . fst $ g) showGrid (snd g)) results

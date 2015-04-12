module Main where

import HSudoku
import Data.Maybe (fromJust, isJust)

main :: IO ()
main = do
    content <- getContents
    let grids = map parseGrid (lines content)
    let result = [r | g <- grids, let t = trySolve .fromJust $ g, let r = if isJust g then t else Nothing]
    mapM_ (putStrLn . \g -> case g of
                                Just x  -> showGrid x
                                Nothing -> []) result

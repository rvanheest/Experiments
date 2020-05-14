module WeIrDsTrInGcAsE where

import Data.Char

zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex = (flip zip) [0..]

weirdCase :: (Char, Int) -> Char
weirdCase (c, i)
    | i `mod` 2 == 0 = toUpper c
    | otherwise = toLower c

toWeirdCase :: String -> String
toWeirdCase = unwords . map (map weirdCase . zipWithIndex) . words

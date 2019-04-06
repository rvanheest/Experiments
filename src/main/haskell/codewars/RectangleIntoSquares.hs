module RectangleIntoSquares where

squaresInRect :: Integer -> Integer -> Maybe [Integer]
squaresInRect lng wdth
    | lng == wdth = Nothing
    | otherwise = Just $ reverse $ recursive lng wdth []
    where recursive x y xs
            | x  < y = recursive x (y - x) (x : xs)
            | x  > y = recursive (x - y) y (y : xs)
            | otherwise = x : xs

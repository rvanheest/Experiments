module Scramblies where

import Data.List

scramble :: String -> String -> Bool
scramble s1 s2 = recursive (sort s1) (sort s2)
    where recursive [] [] = True
          recursive [] _  = False
          recursive _ []  = True
          recursive (x:xs) (y:ys)
              | x  < y    = recursive xs (y:ys)
              | x  > y    = False
              | otherwise = recursive xs ys

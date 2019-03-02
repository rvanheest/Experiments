module ShortestWord where

find_shortest :: String -> Integer
find_shortest = toInteger . minimum . map length . words


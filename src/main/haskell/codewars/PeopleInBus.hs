module PeopleInBus where

number :: [(Int, Int)] -> Int
number = foldl change 0
    where change count (i, o) = count + i - o

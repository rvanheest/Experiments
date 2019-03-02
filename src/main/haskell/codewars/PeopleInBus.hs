module PeopleInBus where

number :: [(Int, Int)] -> Int
number xs = foldl change 0 xs
    where change count (i, o) = count + i - o

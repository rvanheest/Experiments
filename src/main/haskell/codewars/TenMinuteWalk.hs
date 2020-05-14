module TenMinuteWalk where

count :: Eq a => a -> [a] -> Int
count a xs = length $ filter (a ==) xs

isValidWalk :: String -> Bool
isValidWalk walk = length (take 11 walk) == 10 && ns == ss && es == ws
    where ns = count 'n' walk
          ss = count 's' walk
          es = count 'e' walk
          ws = count 'w' walk

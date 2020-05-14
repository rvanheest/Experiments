module FizzBuzz where

main = mapM_ print (fmap convert [1..100])

convert :: Int -> String
convert x | string == "" = show x
          | otherwise    = string
    where string = foldr (\f -> \y -> f(x) ++ y) "" specials
          specials = (special 3 "Fizz") : (special 5 "Buzz") : []

special :: Int -> String -> Int -> String
special divider name = \x -> if (x `mod` divider == 0) then name else ""


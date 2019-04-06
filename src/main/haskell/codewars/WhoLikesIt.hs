module WhoLikesIt where

likes :: [String] -> String
likes [] = "no one likes this"
likes [p1] = p1 ++ " likes this"
likes [p1, p2] = p1 ++ " and " ++ p2 ++ " like this"
likes [p1, p2, p3] = p1 ++ ", " ++ likes [p2, p3]
likes (p1 : p2 : ps) = p1 ++ ", " ++ p2 ++ " and " ++ show (length ps) ++ " others like this"

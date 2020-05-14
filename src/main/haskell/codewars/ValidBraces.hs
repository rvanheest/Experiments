module ValidBraces where

checkBraces :: String -> [Char] -> Bool
checkBraces "" [] = True
checkBraces (c@('(') : tail) mem = checkBraces tail (c : mem)
checkBraces (c@('[') : tail) mem = checkBraces tail (c : mem)
checkBraces (c@('{') : tail) mem = checkBraces tail (c : mem)
checkBraces (c@(')') : tail) ('(' : mem) = checkBraces tail mem
checkBraces (c@(']') : tail) ('[' : mem) = checkBraces tail mem
checkBraces (c@('}') : tail) ('{' : mem) = checkBraces tail mem
checkBraces _ _ = False

validBraces :: String -> Bool
validBraces xs = checkBraces xs []

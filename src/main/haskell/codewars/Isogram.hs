module Isogram where

import Data.List
import Data.Char

isIsogram :: String -> Bool
isIsogram s = (== length lowerS) $ length $ nub lowerS
    where lowerS = map toLower s

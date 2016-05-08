module MaybeTransExperiments where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

getMaybeT :: Num a => MaybeT [] a
getMaybeT = lift [1,2,3]

doStuff :: Num a => MaybeT [] a
doStuff = do
    int <- getMaybeT
    return (2 * int)

scalaSample :: MaybeT [] Integer
scalaSample = do
    x <- lift [0..10]
    guard (x `mod` 2 == 0)
    return (2 * x)

nonDo :: Num a => MaybeT [] a
nonDo = (lift [1,2,3]) >>= (\x -> return x)

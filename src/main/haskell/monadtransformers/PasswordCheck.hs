-- based on https://www.schoolofhaskell.com/user/commercial/content/monad-transformers
module PasswordCheck where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

main :: IO ()
main = do
    password <- runMaybeT getPassword
    case password of
        Just _ -> putStrLn "valid password"
        Nothing -> putStrLn "invalid password"

isValid :: String -> Bool
isValid s = length s >= 10

getPassword :: MaybeT IO String
getPassword = do
    password <- lift getLine
    guard (isValid password)
    return password

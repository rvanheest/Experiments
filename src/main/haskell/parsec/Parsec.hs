module Parsec where

import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Monad

newtype Parser s a = Prsr(s -> Maybe (a, s))

parse (Prsr f) = f

run :: Parser s a -> s -> a
run parser input = case parse parser input of
    Nothing -> undefined
    Just (out, _) -> out

instance Functor (Parser s) where
    fmap f parser = Prsr $ \s -> fmap (\(a, s2) -> (f a, s2)) $ parse parser s

instance Applicative (Parser s) where
    pure a = Prsr(\s -> Just (a, s))
    parser1 <*> parser2 = Prsr $ \s ->
        do (aToB, out) <- parse parser1 s
           (v, out2) <- parse parser2 out
           return (aToB v, out2)

instance Alternative (Parser s) where
    empty = Prsr(\_ -> Nothing)
    parser1 <|> parser2 = Prsr $ \s -> parse parser1 s <|> parse parser2 s

instance Monad (Parser s) where
    return = pure
    parser >>= f = Prsr $ \s -> parse parser s >>= \(v, out) -> parse (f v) out

instance MonadPlus (Parser s) where
    mzero = empty
    mplus = (<|>)

satisfy :: Parser s a -> (a -> Bool) -> Parser s a
satisfy parser pred = parser >>= \x -> if pred x then return x else empty

noneOf :: (Eq a) => Parser s a -> [a] -> Parser s a
noneOf parser xs = satisfy parser (\x -> not (elem x xs))

maybe :: Parser s a -> Parser s (Maybe a)
maybe parser = fmap Just parser <|> return Nothing

takeUntil :: Parser s a -> (a -> Bool) -> Parser s [a]
takeUntil parser pred = many $ satisfy parser (not . pred)

takeWhile :: Parser s a -> (a -> Bool) -> Parser s [a]
takeWhile parser pred = many $ satisfy parser pred

skipMany :: Parser s a -> Parser s ()
skipMany parser = (parser >> skipMany parser) <|> return ()

-- operators and stuff on Parser

type StringParser a = Parser String a

item :: StringParser Char
item = Prsr(\s -> case s of
    [] -> Nothing
    (x:xs) -> Just (x, xs))

digit :: StringParser Char
digit = item `satisfy` isDigit

number :: StringParser String
number = some digit

lower :: StringParser Char
lower = item `satisfy` isLower

upper :: StringParser Char
upper = item `satisfy` isUpper

letter :: StringParser Char
letter = item `satisfy` isLetter

alphanum :: StringParser Char
alphanum = item `satisfy` (\c -> isLetter c || isDigit c)

char :: Char -> StringParser Char
char c = item `satisfy` (c ==)

space :: StringParser Char
space = char ' '

string :: String -> StringParser String
string s = case s of
    [] -> return ""
    (x:xs) -> (char x) >> (string xs) >> (return s)

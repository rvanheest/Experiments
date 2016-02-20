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

spaces :: StringParser ()
spaces = skipMany space

string :: String -> StringParser String
string s = case s of
    [] -> return ""
    (x:xs) -> (char x) >> (string xs) >> (return s)

type AttrName = String
type AttrVal = String

data Attribute = Attribute (AttrName, AttrVal) deriving (Show)

data XML = Element String [Attribute] [XML]
         | SelfClosingTag String [Attribute]
         | Decl String
         | Body String
        deriving (Show)

-- The top level document, used in parsing
document :: StringParser [XML]
document = do
  spaces
  y <- xmlDecl <|> tag
  spaces
  x <- many tag
  spaces
  return (y : x)

-- XML declaration eg. <?xml version="1.0" encoding="UTF-8"?>
xmlDecl :: StringParser XML
xmlDecl = do string "<?xml"
             decl <- many (noneOf item "?>")
             string "?>"
             return (Decl decl)

tag :: StringParser XML
tag = do char '<'
         spaces
         name <- many (letter <|> digit)
         spaces
         attr <- many attribute
         spaces
         close <- string "/>" <|> string ">"
         if (length close) == 2
         then return (SelfClosingTag name attr)
         else do elementBody <- many elementBody
                 endTag name
                 spaces
                 return (Element name attr elementBody)

elementBody :: StringParser XML
elementBody = spaces *> tag <|> text

endTag :: String -> StringParser String
endTag str = string "</" *> string str <* char '>'

text :: StringParser XML
text = Body <$> some (noneOf item "><")

attribute :: StringParser Attribute
attribute = do name <- many (noneOf item "= />")
               spaces
               char '='
               spaces
               char '"'
               value <- many (noneOf item ['"'])
               char '"'
               spaces
               return (Attribute (name, value))

-- testing the xml parser
testString1 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><rss version=\"2.0\">foobar</rss>"
testString2 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> <rss version=\"2.0\">   <channel>     <title>The Register</title>     <link>http://www.theregister.co.uk/</link>     <description>Biting the hand that feeds IT</description>     <copyright>Copyright 2012, Situation Publishing</copyright>     <image>       <title>The Register</title>       <width>88</width>       <height>31</height>       <url>http://www.theregister.co.uk/Design/graphics/Reg_default/The_Register_RSS.png</url>       <link>http://www.theregister.co.uk/</link>     </image>     <language>en-GB</language>     <webMaster>webmaster@theregister.co.uk</webMaster>     <lastBuildDate>Fri, 12 Oct 2012 18:58:41 GMT</lastBuildDate>     <ttl>120</ttl>  <item><guid isPermaLink=\"false\">tag:theregister.co.uk,2005:story/2012/10/12/rbs_santander_incompatible_it_systems_collapse/</guid><title>Incompatible IT systems blamed for bank sale collapse</title><link>http://go.theregister.com/feed/www.theregister.co.uk/2012/10/12/rbs_santander_incompatible_it_systems_collapse/</link><description><h4>RBS £1.7bn branch sale to Santander is off</h4> <p><strong>Brief</strong> Royal Bank of Scotland's $1.7bn sale of 318 branches to Santander has gone titsup.…</p></description><pubDate>Fri, 12 Oct 2012 18:51:54 GMT</pubDate></item>   </channel> </rss>"

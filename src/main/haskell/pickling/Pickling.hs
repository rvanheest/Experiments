module Pickling where

import Data.Maybe

-- based on https://www.microsoft.com/en-us/research/wp-content/uploads/2004/01/picklercombinators.pdf

data PU a = PU {
    appP :: (a, [Char]) -> [Char],
    appU :: [Char] -> (a, [Char])
}

pickle :: PU a -> a -> String
pickle pa a = appP pa (a, [])

unpickle :: PU a -> String -> a
unpickle pa s = fst (appU pa s)

base :: Int
base = 256

belowBase :: PU Int
belowBase = PU (\ (n, s) -> toEnum n : s)
               (\ (c:s) -> (fromEnum c, s))

lift :: a -> PU a
lift x = PU snd (\s -> (x, s))

sequ :: (b -> a) -> PU a -> (a -> PU b) -> PU b
sequ f pa k = PU (\ (b,s) -> let a = f b
                                 pb = k a
                             in appP pa (a, appP pb (b, s)))
                 (\ s -> let (a, s') = appU pa s
                             pb = k a
                         in appU pb s')

pair :: PU a -> PU b -> PU (a, b)
pair pa pb = sequ fst pa (\ a ->
             sequ snd pb (\ b ->
             lift (a, b)))

triple :: PU a -> PU b -> PU c -> PU (a, b, c)
triple pa pb pc = sequ (\ (x, y, z) -> x) pa (\a ->
                  sequ (\ (x, y, z) -> y) pb (\b ->
                  sequ (\ (x, y, z) -> z) pc (\c ->
                  lift (a,b,c))))

quad :: PU a -> PU b -> PU c -> PU d -> PU (a, b, c, d)
quad pa pb pc pd = sequ (\ (w, x, y, z) -> w) pa (\a ->
                   sequ (\ (w, x, y, z) -> x) pb (\b ->
                   sequ (\ (w, x, y, z) -> y) pc (\c ->
                   sequ (\ (w, x, y, z) -> z) pd (\d ->
                   lift (a,b,c,d)))))

wrap :: (a -> b, b -> a) -> PU a -> PU b
wrap (i,j) pa = sequ j pa (lift . i)

zeroTo :: Int -> PU Int
zeroTo 0 = lift 0
zeroTo n = wrap (\ (hi,lo) -> hi * base + lo, (`divMod` base))
                (pair (zeroTo (n `div` base)) belowBase)

unit :: PU ()
unit = lift ()

char :: PU Char
char = wrap (toEnum, fromEnum) (zeroTo 255)

bool :: PU Bool
bool = wrap (toEnum, fromEnum) (zeroTo 1)

nat :: PU Int
nat = sequ (\x -> if x < half then x else half + x `mod` half)
           belowBase
           (\lo -> if lo < half then lift lo
                                else wrap (\hi -> hi * half + lo, \n -> n `div` half - 1) nat)
      where half = base `div` 2

fixedList :: PU a -> Int -> PU [a]
fixedList pa 0 = lift []
fixedList pa n = wrap (\(a, b) -> a:b, \(a:b) -> (a, b)) (pair pa (fixedList pa (n-1)))

list :: PU a -> PU [a]
list = sequ length nat . fixedList

string :: PU String
string = list char

alt :: (a -> Int) -> [PU a] -> PU a
alt tag ps = sequ tag (zeroTo (length ps-1)) (ps !!)

pMaybe :: PU a -> PU (Maybe a)
pMaybe pa = alt tag [lift Nothing, wrap (Just, fromJust) pa]
            where tag Nothing = 0
                  tag (Just x) = 1

pEither :: PU a -> PU b -> PU (Either a b)
pEither pa pb = alt tag [wrap (Left, fromLeft) pa, wrap (Right, fromRight) pb]
                where tag (Left _) = 0
                      tag (Right _) = 1
                      fromLeft (Left a) = a
                      fromRight (Right b) = b



data URL = URL { protocol :: String, host :: String, port :: Maybe Int, file :: String } deriving Show

url :: PU URL
url = wrap (\ (pr, h, po, f) -> URL {protocol = pr, host = h, port = po, file = f},
            \ URL {protocol = pr,host = h,port = po,file = f} -> (pr, h, po, f))
           (quad string string (pMaybe nat) string)

data Bookmark = Link (String, URL) | Folder (String, Bookmarks) deriving Show

bookmark :: PU Bookmark
bookmark = alt tag [wrap (Link, \(Link a) -> a) (pair string url),
                    wrap (Folder, \(Folder a) -> a) (pair string bookmarks)]
           where tag (Link _) = 0
                 tag (Folder _) = 1

type Bookmarks = [Bookmark]

bookmarks :: PU Bookmarks
bookmarks = list bookmark


urlExample :: URL
urlExample = URL {
                 protocol = "http",
                 host = "research.microsoft.com",
                 port = Nothing,
                 file = "users/akenn"
             }

linkExample :: Bookmark
linkExample = Link ("Andrew", urlExample)

example :: Bookmarks
example = [linkExample]

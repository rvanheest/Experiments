{-# LANGUAGE ExistentialQuantification #-}
module Average where

import Data.Monoid hiding (Sum, getSum)
import Data.Foldable

--data Sum a = Sum a deriving Show
newtype Sum a = Sum { getSum :: a } deriving (Show)

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0
  mappend l r = Sum (getSum l + getSum r)

data Count = Count Int deriving Show

instance Monoid Count where
  mempty = Count 0
  mappend (Count l) (Count r) = Count (l + r)

data Average a = Average (Sum a, Count) deriving (Show)

instance (Num a) => Monoid (Average a) where
  mempty = Average mempty
  mappend (Average l) (Average r) = Average (mappend l r)

average :: (Fractional a) => Average a -> a
average (Average (Sum s, Count c)) = s / fromIntegral c

calculateAverage :: (Fractional a) => [a] -> a
calculateAverage = average . foldMap (\x -> Average (Sum x, Count 1))

data Variance a = Variance (Sum a, Sum a, Count) deriving (Show)

instance (Num a) => Monoid (Variance a) where
  mempty = Variance mempty
  mappend (Variance l) (Variance r) = Variance (mappend l r)

variance :: (Fractional a) => Variance a -> a
variance (Variance (Sum sq, Sum s, c)) = averageOfSquares - (averageOfSums * averageOfSums)
  where averageOfSquares = average (Average (Sum sq, c))
        averageOfSums = average (Average (Sum s, c))

calculateVariance :: (Fractional a) => [a] -> a
calculateVariance = variance . foldMap (\x -> Variance (Sum (x * x), Sum x, Count 1))

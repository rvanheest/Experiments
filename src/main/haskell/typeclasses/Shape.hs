{-# LANGUAGE MultiParamTypeClasses #-}
module Shape where

data Square = Square Int Int Float deriving Show
data Rectangle = Rectangle Int Int Float Float deriving Show
data Circle = Circle Int Int Float deriving Show
data Ellipse = Ellipse Int Int Float Float deriving Show

class Shape x
instance Shape Square
instance Shape Rectangle
instance Shape Circle
instance Shape Ellipse

class (Shape x, Shape y) => Intersect x y where
    intersect :: x -> y -> Bool

instance Intersect Square Square where
    intersect s s' = undefined

instance Intersect Rectangle Rectangle where
    intersect r r' = undefined

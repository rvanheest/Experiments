{-# LANGUAGE ExistentialQuantification #-}
module Expr where

class Expr e where
    eval :: e -> Int

data Number = Number Int
data Plus a b = (Expr a, Expr b) => Plus a b

instance Expr Number where
    eval (Number n) = n

instance (Expr a, Expr b) => Expr (Plus a b) where
    eval (Plus x y) = (eval x) + (eval y)

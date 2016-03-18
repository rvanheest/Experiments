{-# LANGUAGE ExistentialQuantification #-}
module ExprWithNegate where
import Expr

data Negate a = (Expr a) => Negate a

instance (Expr a) => Expr (Negate a) where
    eval (Negate x) = -eval x

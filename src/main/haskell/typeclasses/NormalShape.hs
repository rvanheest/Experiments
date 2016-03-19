{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module NormalShape where
import Shape

class Shape s => NormalShape s
instance NormalShape Square
instance NormalShape Circle

class (Shape s1, NormalShape s2) => Normalize s1 s2 | s1 -> s2 where
    normalize :: s1 -> s2

instance Normalize Square Square where
    normalize = id

instance Normalize Circle Circle where
    normalize = id

instance Normalize Rectangle Square where
    normalize (Rectangle x y w h) = Square x y (sqrt(w * h))

instance Normalize Ellipse Circle where
    normalize = undefined

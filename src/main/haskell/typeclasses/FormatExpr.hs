module FormatExpr where
import Expr

class (Expr t) => FormatExpr t where
    format :: t -> String

instance FormatExpr Number where
    format (Number n) = show n

instance (FormatExpr a, FormatExpr b) => FormatExpr (Plus a b) where
    format (Plus x y) = "(" ++ (format x) ++ " + " ++ format(y) ++ ")"

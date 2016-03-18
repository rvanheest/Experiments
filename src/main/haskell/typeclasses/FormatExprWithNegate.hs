module FormatExprWithNegate where
import FormatExpr
import ExprWithNegate

instance (FormatExpr a) => FormatExpr (Negate a) where
    format (Negate x) = "-" ++ (format x)

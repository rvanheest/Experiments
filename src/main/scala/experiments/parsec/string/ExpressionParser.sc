import experiments.parsec.Parser
import experiments.parsec.string.StringParser._

def expr = (for {
  a <- term
  _ <- char('+')
  b <- term
} yield a + b) <|> term

def term = (for {
  a <- factor
  _ <- char('*')
  b <- factor
} yield a * b) <|> factor

def factor: Parser[String, Int] = number.map(_.toInt) <|> (for {
  _ <- char('(')
  a <- expr
  _ <- char(')')
} yield a)

expr.eval("1+1")
expr.eval("2*3+1")
expr.eval("(2+3)*4")
expr.eval("2+3*4")
expr.eval("(2*3)+(2+7)")

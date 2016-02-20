package experiments.parsec

class Parser[S, A](val parse: S => Option[(A, S)]) {

	def run(input: S): A = {
		parse(input) map (_._1) getOrElse sys.error("No result")
	}
}
object Parser {
	def apply[S, A](parser: S => Option[(A, S)]) = new Parser(parser)

	def from[S, A](a: A): Parser[S, A] = Parser(Some(a, _))

	def failure[S, A]: Parser[S, A] = Parser(_ => None)
}

package experiments.pickling

trait example2 extends StringPickler {

	sealed abstract class Lambda
	case class Var(s: String) extends Lambda
	case class Lam(s: String, l: Lambda) extends Lambda
	case class App(left: Lambda, right: Lambda) extends Lambda

	def lambda: PU[Lambda] = {
		def tag(lambda: Lambda): Int = {
			lambda match {
				case Var(_) => 0
				case Lam(_, _) => 1
				case App(_, _) => 2
			}
		}

		alt(List(
			string.wrap[Lambda](Var)({ case Var(s) => s}),
			string.pair(lambda).wrap[Lambda]({ case (s, l) => Lam(s, l) })({ case Lam(s, l) => (s, l) }),
			lambda.pair(lambda).wrap[Lambda]({ case (l1, l2) => App(l1, l2) })({ case App(l1, l2) => (l1, l2) })
		))(tag)
	}
}

object example2 extends App with example2 {


}

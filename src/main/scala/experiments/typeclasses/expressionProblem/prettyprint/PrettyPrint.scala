package experiments.typeclasses.expressionProblem.prettyprint

trait PrettyPrint[E] {
  def format(e: E): String
}
object PrettyPrint {
  def apply[E](implicit E: PrettyPrint[E]): PrettyPrint[E] = E

  def from[E](f: E => String): PrettyPrint[E] = f(_)
}

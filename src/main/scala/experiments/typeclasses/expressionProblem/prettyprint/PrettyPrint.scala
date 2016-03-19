package experiments.typeclasses.expressionProblem.prettyprint

trait PrettyPrint[E] {
  def format(e: E): String
}

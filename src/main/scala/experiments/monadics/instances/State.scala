package experiments.monadics.instances

class State[S, A](val state: S => (A, S)) {
  def run(s: S): (A, S) = state(s)
  def evaluate(s: S): A = state(s)._1
  def execute(s: S): S = state(s)._2
}

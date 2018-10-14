package experiments.effect.structuredMessages

case class State[S, A](state: S => (A, S)) {

  def run(s: S): (A, S) = state(s)

  def evaluate(s: S): A = state(s)._1

  def execute(s: S): S = state(s)._2

  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s2) = state(s)
    (f(a), s2)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = state(s)
    val state2 = f(a)
    state2.run(s2)
  })
}

object State {
  def from[S, A](a: => A): State[S, A] = State((a, _))
}

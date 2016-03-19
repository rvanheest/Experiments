package experiments.monadics.instances

class State[S, A](val state: S => (A, S)) {
  def run(s: S): A = state(s)._1
}

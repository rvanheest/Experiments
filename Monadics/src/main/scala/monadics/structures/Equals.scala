package monadics.structures

trait Equals[A] {
  def equals(x: A, y: A): Boolean
}

object Equals {

  def natural[A]: Equals[A] = create(_ == _)

  def create[A](f: (A, A) => Boolean): Equals[A] = new Equals[A] {
    def equals(x: A, y: A): Boolean = f(x, y)
  }
}

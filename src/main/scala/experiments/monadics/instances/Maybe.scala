package experiments.monadics.instances

sealed abstract class Maybe[A]
object Maybe {
  def apply[A](a: A): Maybe[A] = Just(a)
  def empty[A]: Maybe[A] = Nothing[A]()
}
case class Just[A](a: A) extends Maybe[A]
case class Nothing[A]() extends Maybe[A]

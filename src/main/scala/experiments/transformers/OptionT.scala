package experiments.transformers

import experiments.monadics.Monad

import scala.language.higherKinds

case class OptionT[M[_], A](run: M[Option[A]])(implicit m: Monad[M]) {
  import OptionT.lift

  def map[B](f: A => B): OptionT[M, B] = {
    lift(m.map(run)(_ map f))
  }

  def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] = {
    lift(m.flatMap(run)(_ map f.andThen(_.run) getOrElse m.create(None)))
  }

  def filter(predicate: A => Boolean): OptionT[M, A] = {
    lift(m.map(run)(_ filter predicate))
  }
}
object OptionT {
  def lift[M[_], A](x: M[Option[A]])(implicit m: Monad[M]): OptionT[M, A] = new OptionT[M, A](x)
}

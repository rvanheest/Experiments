package monadics.instances.monoids

import monadics.structures.{Alternative, Monoid}

import scala.language.higherKinds

case class Alt[F[_], A](alt: F[A])(implicit monoid: Monoid[Alt[F, A]], alternative: Alternative[F]) {

  def orElse(other: => Alt[F, A]): Alt[F, A] = {
    monoid.combine(this, other)
  }
}

object Alt {
  def empty[F[_], A](implicit monoid: Monoid[Alt[F, A]], alternative: Alternative[F]): Alt[F, A] = {
    monoid.empty
  }

  implicit def altIsMonoid[F[_], A](implicit alternative: Alternative[F]): Monoid[Alt[F, A]] = {
    Monoid.create(Alt(alternative.empty[A])) {
      case (Alt(x), Alt(y)) => Alt(alternative.orElse(x, y))
    }
  }
}

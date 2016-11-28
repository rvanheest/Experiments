package monadics.instances

import monadics.structures._

import scala.language.higherKinds

trait option {

  implicit def optionOfSemigroupIsMonoid[A](implicit monoid: Semigroup[A]): Monoid[Option[A]] = Monoid.create(Option.empty[A]) {
    case (None, None) => None
    case (a@Some(_), None) => a
    case (None, b@Some(_)) => b
    case (Some(x), Some(y)) => Some(monoid.combine(x, y))
  }

  implicit val optionIsMonadPlusAndMonadFail = new MonadPlus[Option] with MonadFail[Option] with Traverse[Option] {
    def empty[A]: Option[A] = Option.empty

    def create[A](a: A): Option[A] = Option(a)

    def fail[A](e: Throwable): Option[A] = Option.empty

    override def map[A, B](option: Option[A])(f: A => B): Option[B] = {
      option.map(f)
    }

    def flatMap[A, B](option: Option[A])(f: A => Option[B]): Option[B] = {
      option.flatMap(f)
    }

    def orElse[A, B >: A](option1: Option[A], option2: => Option[B]): Option[B] = {
      option1.orElse(option2)
    }

    override def foldLeft[A, B](option: Option[A], z: => B)(f: (=> B, A) => B): B = {
      option.map(f(z, _)).getOrElse(z)
    }

    override def foldRight[A, B](option: Option[A], z: => B)(f: (A, => B) => B): B = {
      option.map(f(_, z)).getOrElse(z)
    }

    def traverse[G[_], A, B](optA: Option[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[Option[B]] = {
      optA.map(a => applicative.map(f(a))(Option(_)))
        .getOrElse(applicative.create(Option.empty[B]))
    }

    override def sequence[G[_], A](optGA: Option[G[A]])(implicit applicative: Applicative[G]): G[Option[A]] = {
      optGA.map(ga => applicative.map(ga)(Option(_)))
        .getOrElse(applicative.create(Option.empty[A]))
    }
  }

  implicit class OptionMonoid[A: Semigroup](val option: Option[A])(implicit monoid: Monoid[Option[A]]) {
    def combine(other: => Option[A]): Option[A] = monoid.combine(option, other)
  }

  implicit class OptionMonadPlusOperators[A](val option: Option[A])(implicit monadTraverse: MonadPlus[Option] with Traverse[Option]) {
    def as[B](b: => B): Option[B] = monadTraverse.as(option, b)

    def void: Option[Unit] = monadTraverse.void(option)

    def zipWith[B](f: A => B): Option[(A, B)] = monadTraverse.zipWith(option)(f)

    def traverse[G[_], B](f: A => G[B])(implicit applicative: Applicative[G]): G[Option[B]] = {
      monadTraverse.traverse(option)(f)
    }

    def sequence[G[_], B](implicit ev: A <:< G[B], applicative: Applicative[G]): G[Option[B]] = {
      monadTraverse.sequence(option.map(ev))
    }
  }
}

object option extends option

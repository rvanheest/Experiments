package monadics.instances

import monadics.structures._

trait option {

  implicit def optionOfSemigroupIsMonoid[A](implicit monoid: Semigroup[A]): Monoid[Option[A]] = Monoid.create(Option.empty[A]) {
    case (None, None) => None
    case (a@Some(_), None) => a
    case (None, b@Some(_)) => b
    case (Some(x), Some(y)) => Some(monoid.combine(x, y))
  }

  implicit class OptionMonoid[A: Semigroup](val option: Option[A])(implicit monoid: Monoid[Option[A]]) {
    def combine(other: => Option[A]): Option[A] = monoid.combine(option, other)
  }

  implicit def optionIsMonadPlusAndMonadFail = new MonadPlus[Option] with MonadFail[Option] {
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
  }

  implicit class OptionMonadPlusOperators[A](val option: Option[A])(implicit monadPlus: MonadPlus[Option]) {
    def as[B](b: => B): Option[B] = monadPlus.as(option, b)

    def void: Option[Unit] = monadPlus.void(option)

    def zipWith[B](f: A => B): Option[(A, B)] = monadPlus.zipWith(option)(f)
  }

  implicit def optionIsFoldable = new Foldable[Option] {
    override def foldLeft[A, B](option: Option[A], z: => B)(f: (=> B, A) => B): B = {
      option.map(f(z, _)).getOrElse(z)
    }

    override def foldRight[A, B](option: Option[A], z: => B)(f: (A, => B) => B): B = {
      option.map(f(_, z)).getOrElse(z)
    }
  }
}

object option extends option

package monadics.instances

import monadics.structures.{MonadFail, MonadPlus, Monoid, Semigroup}

trait option {

  implicit def optionOfSemigroupIsMonoid[A](implicit monoid: Semigroup[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    def empty: Option[A] = Option.empty

    def combine(optA: Option[A], optB: => Option[A]): Option[A] = {
      optA.map(a =>
        optB.map(b => Some(monoid.combine(a, b)))
          .getOrElse(optA))
        .getOrElse(optB)
    }
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
}

object option extends option

package monadics.structures

import monadics.instances.monoids._

import scala.language.{higherKinds, postfixOps}

trait Foldable[F[_]] {
  def foldLeft[A, B](fa: F[A], z: => B)(f: (=> B, A) => B): B = {
    implicit val monoid = Monoid.create[B => B](identity)(_ compose _)
    val fb = foldMap(fa)(a => (b: B) => f(b, a))
    fb(z)
  }

  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B = {
    implicit val monoid = Monoid.create[B => B](identity)(_ andThen _)
    val fb = foldMap(fa)(a => (b: B) => f(a, b))
    fb(z)
  }

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit mb: Monoid[B]): B = {
    foldLeft(fa, mb.empty)((b, a) => mb.combine(b, f(a)))
  }

  def fold[A](fa: F[A])(implicit ma: Monoid[A]): A = {
    foldMap(fa)(identity)
  }

  def toList[A](fa: F[A]): List[A] = {
    foldRight(fa, List.empty[A])(_ :: _)
  }

  def isEmpty[A](fa: F[A]): Boolean = {
    foldRight(fa, true)((_, _) => false)
  }

  def size[A](fa: F[A]): Int = {
    foldLeft(fa, 0)((b, _) => b + 1)
  }

  def contains[A](fa: F[A])(a: A): Boolean = {
    exists(fa)(a ==)
  }

  def max[A](fa: F[A])(implicit ordered: Ordering[A]): Option[A] = {
    foldLeft(fa, Option.empty[A])((acc, a) => acc.map(ac => Option(ordered.max(ac, a))).getOrElse(Option(a)))
  }

  def min[A](fa: F[A])(implicit ordered: Ordering[A]): Option[A] = {
    foldLeft(fa, Option.empty[A])((acc, a) => acc.map(ac => Option(ordered.min(ac, a))).getOrElse(Option(a)))
  }

  def sum[A](fa: F[A])(implicit numeric: Numeric[A]): A = {
    foldMap(fa)(Sum(_)).sum
  }

  def product[A](fa: F[A])(implicit numeric: Numeric[A]): A = {
    foldMap(fa)(Product(_)).product
  }

  def all(fBool: F[Boolean]): Boolean = {
    foldMap(fBool)(All(_)).all
  }

  def any(fBool: F[Boolean]): Boolean = {
    foldMap(fBool)(Any(_)).any
  }

  def exists[A](fa: F[A])(predicate: A => Boolean): Boolean = {
    foldMap(fa)(predicate andThen (Any(_))).any
  }

  def forall[A](fa: F[A])(predicate: A => Boolean): Boolean = {
    foldMap(fa)(predicate andThen (All(_))).all
  }

  def find[A](fa: F[A])(predicate: A => Boolean): Option[A] = {
    foldMap(fa)(a => First(if (predicate(a)) Option(a) else Option.empty)).first
  }

//  def traverse_[App[_], A, B](fa: F[A])(f: A => App[B])(implicit applicative: Applicative[App]): App[Unit] = {
//    foldRight(fa, applicative.create(()))((a, b) => applicative.*>(f(a), b))
//  }

//  def concat[Alt[_], A](fAltA: F[Alt[A]])(implicit alternative: Alternative[Alt]): Alt[A] = {
//    foldRight(fAltA, alternative.empty[A])(alternative.orElse(_, _))
//  }
}

/*
  examples:
    List(Option(1), Option(2), Option(3)).fold
      // should be Some(6)
    List(Option(1), Option(2), Option(3)).foldMap(o => o)
      // should be Some(6)
    size == xs.foldMap(_ => 1)
    union keys in List of Maps
 */

package monadics.instances

import monadics.structures._

import scala.language.higherKinds

case class NonEmptyList[A](head: A, tail: List[A])(implicit semigroup: Semigroup[NonEmptyList[A]], monadTraverse: Monad[NonEmptyList] with Traverse[NonEmptyList]) {

  def map[B](f: A => B): NonEmptyList[B] = monadTraverse.map(this)(f)

  def as[B](b: => B): NonEmptyList[B] = monadTraverse.as(this, b)

  def void: NonEmptyList[Unit] = monadTraverse.void(this)

  def zipWith[B](f: A => B): NonEmptyList[(A, B)] = monadTraverse.zipWith(this)(f)

  def <*>[B, C](other: NonEmptyList[B])(implicit ev: NonEmptyList[A] <:< NonEmptyList[(B => C)]): NonEmptyList[C] = monadTraverse.<*>(this, other)

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] = monadTraverse.flatMap(this)(f)

  def ++(other: NonEmptyList[A]): NonEmptyList[A] = semigroup.combine(this, other)

  def foldLeft[B](z: => B)(f: (=> B, A) => B): B = monadTraverse.foldLeft(this, z)(f)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = monadTraverse.foldRight(this, z)(f)

  def foldMap[B](f: A => B)(implicit mb: Monoid[B]): B = monadTraverse.foldMap(this)(f)

  def toList: List[A] = monadTraverse.toList(this)

  def size: Int = monadTraverse.size(this)

  def contains(a: A): Boolean = monadTraverse.contains(this)(a)

  def max(implicit ordered: Ordering[A]): A = monadTraverse.max(this).get

  def min(implicit ordered: Ordering[A]): A = monadTraverse.min(this).get

  def sum(implicit numeric: Numeric[A]): A = monadTraverse.sum(this)

  def product(implicit numeric: Numeric[A]): A = monadTraverse.product(this)

  def all(implicit ev: NonEmptyList[A] <:< NonEmptyList[Boolean]): Boolean = monadTraverse.all(this)

  def any(implicit ev: NonEmptyList[A] <:< NonEmptyList[Boolean]): Boolean = monadTraverse.any(this)

  def exists(predicate: A => Boolean): Boolean = monadTraverse.exists(this)(predicate)

  def forall(predicate: A => Boolean): Boolean = monadTraverse.forall(this)(predicate)

  def find(predicate: A => Boolean): Option[A] = monadTraverse.find(this)(predicate)

  def traverse[G[_], B](f: A => G[B])(implicit applicative: Applicative[G]): G[NonEmptyList[B]] = {
    monadTraverse.traverse(this)(f)
  }

  def sequence[G[_], B](implicit ev: NonEmptyList[A] <:< NonEmptyList[G[B]], applicative: Applicative[G]): G[NonEmptyList[B]] = {
    monadTraverse.sequence[G, B](this)
  }
}

object NonEmptyList {

  def apply[A](head: A, tail: A*): NonEmptyList[A] = {
    new NonEmptyList(head, tail.toList)
  }

  implicit def NELisEquals[A](implicit aEquals: Equals[A], listEquals: Equals[List[A]]): Equals[NonEmptyList[A]] = {
    Equals.create {
      case (NonEmptyList(h1, Nil), NonEmptyList(h2, Nil)) => aEquals.equals(h1, h2)
      case (NonEmptyList(h1, t1), NonEmptyList(h2, t2)) => aEquals.equals(h1, h2) && listEquals.equals(t1, t2)
    }
  }

  implicit def NELisSemigroup[A]: Semigroup[NonEmptyList[A]] = Semigroup.create {
    (a1, a2) => NonEmptyList(a1.head, a1.tail ++ (a2.head :: a2.tail))
  }

  implicit val NELisMonad: Monad[NonEmptyList] with Traverse[NonEmptyList] = new Monad[NonEmptyList] with Traverse[NonEmptyList] {

    def create[A](a: A): NonEmptyList[A] = NonEmptyList(a, Nil)

    def flatMap[A, B](monad: NonEmptyList[A])(f: A => NonEmptyList[B]): NonEmptyList[B] = {
      val NonEmptyList(head, tail) = f(monad.head)

      def foldFunc(nelb: NonEmptyList[B], xs: List[B]): List[B] = {
        (nelb.head :: nelb.tail) ++ xs
      }

      NonEmptyList(head, tail ++ monad.tail.map(f).foldRight(List.empty[B])(foldFunc))
    }

    override def foldLeft[A, B](nel: NonEmptyList[A], z: => B)(f: (=> B, A) => B): B = {
      val NonEmptyList(x, xs) = nel
      xs.foldLeft(f(z, x))((b, a) => f(b, a))
    }

    override def foldRight[A, B](nel: NonEmptyList[A], z: => B)(f: (A, => B) => B): B = {
      val NonEmptyList(x, xs) = nel
      f(x, xs.foldRight(z)((a, b) => f(a, b)))
    }

    override def toList[A](nel: NonEmptyList[A]): List[A] = nel.head :: nel.tail

    override def size[A](nel: NonEmptyList[A]): Int = 1 + nel.tail.size

    override def contains[A](nel: NonEmptyList[A])(a: A): Boolean = nel.head == a || nel.tail.contains(a)

    override def max[A](nel: NonEmptyList[A])(implicit ordered: Ordering[A]): Option[A] = {
      if (nel.tail.isEmpty) Option(nel.head) else Option(ordered.max(nel.head, nel.tail.max))
    }

    override def min[A](nel: NonEmptyList[A])(implicit ordered: Ordering[A]): Option[A] = {
      Option(if (nel.tail.isEmpty) nel.head else ordered.min(nel.head, nel.tail.min))
    }

    override def sum[A](nel: NonEmptyList[A])(implicit numeric: Numeric[A]): A = {
      numeric.plus(nel.head, nel.tail.sum)
    }

    override def product[A](nel: NonEmptyList[A])(implicit numeric: Numeric[A]): A = {
      numeric.times(nel.head, nel.tail.product)
    }

    override def exists[A](nel: NonEmptyList[A])(predicate: A => Boolean): Boolean = {
      predicate(nel.head) || nel.tail.exists(predicate)
    }

    override def forall[A](nel: NonEmptyList[A])(predicate: A => Boolean): Boolean = {
      predicate(nel.head) && nel.tail.forall(predicate)
    }

    override def find[A](nel: NonEmptyList[A])(predicate: A => Boolean): Option[A] = {
      Option(nel.head).filter(predicate).orElse(nel.tail.find(predicate))
    }

    def traverse[G[_], A, B](nel: NonEmptyList[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[NonEmptyList[B]] = {
      nel match {
        case NonEmptyList(x, Nil) => applicative.map(f(x))(b => NonEmptyList(b))
        case NonEmptyList(x, xs) =>
          val gB: G[B] = f(x)
          val gListB: G[List[B]] = xs.foldRight(applicative.create(List.empty[B]))((a: A, acc: G[List[B]]) => {
            applicative.<**>(acc, applicative.map(f(a))((b: B) => (bs: List[B]) => b :: bs))
          })

          applicative.<**>(gB, applicative.map(gListB)((bs: List[B]) => (b: B) => NonEmptyList(b, bs)))
      }
    }
  }
}

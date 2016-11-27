package monadics.instances

import monadics.structures._

import scala.language.higherKinds

trait list {

  implicit def listIsMonoid[A]: Monoid[List[A]] = Monoid.create[List[A]](Nil)(_ ++ _)

  implicit def listIsMonadPlus = new MonadPlus[List] with MonadFail[List] {
    def empty[A]: List[A] = List.empty

    def create[A](a: A): List[A] = List(a)

    def fail[A](e: Throwable): List[A] = List.empty

    override def map[A, B](list: List[A])(f: A => B): List[B] = list.map(f)

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.flatMap(f)

    def orElse[A, B >: A](list1: List[A], list2: => List[B]): List[B] = list1 ++ list2
  }

  implicit def listIsFoldable = new Foldable[List] {
    override def foldLeft[A, B](fa: List[A], z: => B)(f: (=> B, A) => B): B = fa.foldLeft(z)(f(_, _))

    override def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f(_, _))

    override def fold[A](fa: List[A])(implicit ma: Monoid[A]): A = fa.fold(ma.empty)(ma.combine(_, _))

    override def toList[A](fa: List[A]): List[A] = fa

    override def isEmpty[A](fa: List[A]): Boolean = fa.isEmpty

    override def size[A](fa: List[A]): Int = fa.size

    override def contains[A](fa: List[A])(a: A): Boolean = fa.contains(a)

    override def max[A](fa: List[A])(implicit ordered: Ordering[A]): Option[A] = {
      if (isEmpty(fa)) Option.empty[A] else Option(fa.max)
    }

    override def min[A](fa: List[A])(implicit ordered: Ordering[A]): Option[A] = {
      if (isEmpty(fa)) Option.empty[A] else Option(fa.min)
    }

    override def sum[A](fa: List[A])(implicit numeric: Numeric[A]): A = fa.sum

    override def product[A](fa: List[A])(implicit numeric: Numeric[A]): A = fa.product

    override def exists[A](fa: List[A])(predicate: A => Boolean): Boolean = fa.exists(predicate)

    override def forall[A](fa: List[A])(predicate: A => Boolean): Boolean = fa.forall(predicate)

    override def find[A](fa: List[A])(predicate: A => Boolean): Option[A] = fa.find(predicate)
  }

  implicit class ListMonadPlusOperators[A](val list: List[A])(implicit monadPlus: MonadPlus[List]) {
    def as[B](b: => B): List[B] = monadPlus.as(list, b)

    def void: List[Unit] = monadPlus.void(list)

    def zipWith[B](f: A => B): List[(A, B)] = monadPlus.zipWith(list)(f)

    def <*>[B, C](other: List[B])(implicit ev: A <:< (B => C)): List[C] = monadPlus.<*>(list.map(ev), other)
  }

  implicit class ListFoldableOperators[A](val list: List[A])(implicit foldable: Foldable[List]) {
    def foldMap[B](f: A => B)(implicit mb: Monoid[B]): B = foldable.foldMap(list)(f)

    def all(implicit aIsBoolean: A <:< Boolean): Boolean = foldable.all(list.map(aIsBoolean))

    def any(implicit aIsBoolean: A <:< Boolean): Boolean = foldable.any(list.map(aIsBoolean))
  }
}
object list extends list

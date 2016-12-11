package monadics.instances

import monadics.structures._

import scala.language.higherKinds

trait list {

  implicit def listIsEquals[A](implicit aEquals: Equals[A]): Equals[List[A]] = {
    Equals.create((xs, ys) => xs.size == ys.size && xs.zip(ys).forall((aEquals.equals _).tupled))
  }

  implicit def listIsMonoid[A]: Monoid[List[A]] = Monoid.create[List[A]](Nil)(_ ++ _)

  implicit val listIsMonadPlus = new MonadPlus[List] with MonadFail[List] with Traverse[List] {
    def empty[A]: List[A] = List.empty

    def create[A](a: A): List[A] = List(a)

    def fail[A](e: Throwable): List[A] = List.empty

    override def map[A, B](list: List[A])(f: A => B): List[B] = list.map(f)

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.flatMap(f)

    def combine[A, B >: A](list1: List[A], list2: => List[B]): List[B] = list1 ++ list2

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

    def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[List[B]] = {
      fa.foldRight(applicative.create(List.empty[B]))((a: A, acc: G[List[B]]) => {
        applicative.<**>(acc, applicative.map(f(a))((b: B) => (bs: List[B]) => b :: bs))
      })
    }

    override def sequence[G[_], A](fa: List[G[A]])(implicit applicative: Applicative[G]): G[List[A]] = {
      fa.foldRight(applicative.create(List.empty[A]))((ga: G[A], acc: G[List[A]]) => {
        applicative.<**>(acc, applicative.map(ga)((a: A) => (as: List[A]) => a :: as))
      })
    }
  }

  implicit class ListMonadPlusOperators[A](val list: List[A])(implicit monadTraverse: MonadPlus[List] with Traverse[List]) {
    def as[B](b: => B): List[B] = monadTraverse.as(list, b)

    def void: List[Unit] = monadTraverse.void(list)

    def zipWith[B](f: A => B): List[(A, B)] = monadTraverse.zipWith(list)(f)

    def <*>[B, C](other: List[B])(implicit ev: List[A] <:< List[(B => C)]): List[C] = monadTraverse.<*>(list, other)

    def foldMap[B](f: A => B)(implicit mb: Monoid[B]): B = monadTraverse.foldMap(list)(f)

    def all(implicit ev: List[A] <:< List[Boolean]): Boolean = monadTraverse.all(list)

    def any(implicit ev: List[A] <:< List[Boolean]): Boolean = monadTraverse.any(list)

    def traverse[G[_], B](f: A => G[B])(implicit applicative: Applicative[G]): G[List[B]] = {
      monadTraverse.traverse(list)(f)
    }

    def sequence[G[_], B](implicit ev: List[A] <:< List[G[B]], applicative: Applicative[G]): G[List[B]] = {
      monadTraverse.sequence(list)
    }
  }
}
object list extends list

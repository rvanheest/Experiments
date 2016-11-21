package monadics.instances

import monadics.structures.{MonadFail, MonadPlus, Monoid}

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
}
object list extends list

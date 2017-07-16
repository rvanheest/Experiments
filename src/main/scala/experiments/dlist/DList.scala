package experiments.dlist

import java.util.NoSuchElementException

import scala.annotation.tailrec

class DList[A](f: List[A] => List[A]) {

  def run(as: List[A]): List[A] = f(as)

  def toList: List[A] = run(List.empty)

  def ::(a: A): DList[A] = DList(a :: this.run(_))

  def :+(a: A): DList[A] = DList(this.run(_) :+ a)

  def :::(xs: DList[A]): DList[A] = DList(ys => this.run(xs.run(ys)))

  def list[B](b: B)(g: (A, DList[A]) => B): B = {
    this.toList match {
      case Nil => b
      case x :: xs => g(x, DList.fromList(xs))
    }
  }

  def head: A = list(throw new NoSuchElementException("head of empty list"))((a, _) => a)

  def tail: DList[A] = list(throw new NoSuchElementException("head of empty list"))((_, xs) => xs)

  def unfoldRight[B](b: B)(g: B => Option[(A, B)]): DList[A] = {
    g(b).fold(DList.empty[A]) { case (a, b2) => a :: unfoldRight(b2)(g) }
  }

  def foldRight[B](b: B)(g: (A, B) => B): B = this.toList.foldRight(b)(g)

  def map[B](g: A => B): DList[B] = foldRight(DList.empty[B])(g(_) :: _)

  def flatMap[B](g: A => DList[B]): DList[B] = {
    foldRight(DList.empty[B])(g(_) ::: _)
  }
}

object DList {

  def apply[A](f: List[A] => List[A]): DList[A] = new DList(f)

  def fromList[A](as: List[A]): DList[A] = DList[A](xs => xs ++ as)

  def empty[A]: DList[A] = DList(identity)

  def singleton[A](a: A): DList[A] = DList(a :: _)

  def concat[A](xss: List[DList[A]]): DList[A] = xss.foldRight(empty[A])(_ ::: _)

  def fill[A](n: Int)(a: A): DList[A] = DList(xs => {
    @tailrec
    def go(m: Int, result: List[A]): List[A] = {
      if (m <= 0) result
      else go(m - 1, a :: result)
    }

    go(n, xs)
  })
}

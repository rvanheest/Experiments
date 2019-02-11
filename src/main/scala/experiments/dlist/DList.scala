package experiments.dlist

import scala.annotation.tailrec

class DList[A](private val unDL: List[A] => List[A]) {

  def toList: List[A] = unDL(List.empty)

  def ::(a: A): DList[A] = DList(a :: unDL(_))

  def :+(a: A): DList[A] = DList(unDL(_) :+ a)

  def :::(prefix: DList[A]): DList[A] = prefix.append(this)

  def append(xs: DList[A]): DList[A] = DList(ys => xs.unDL(this.unDL(ys)))

  def list[B](b: => B)(g: (A, DList[A]) => B): B = {
    this.toList match {
      case Nil => b
      case x :: xs => g(x, DList.fromList(xs))
    }
  }

  def head: A = list(throw new NoSuchElementException("head of empty list"))((a, _) => a)

  def headOption: Option[A] = list(Option.empty[A])((a, _) => Option(a))

  def tail: DList[A] = list(throw new NoSuchElementException("head of empty list"))((_, xs) => xs)

  def foldRight[B](b: => B)(f: (A, B) => B): B = this.toList.foldRight(b)(f)

  def map[B](f: A => B): DList[B] = foldRight(DList.empty[B])(f(_) :: _)

  def flatMap[B](g: A => DList[B]): DList[B] = foldRight(DList.empty[B])((a, bs) => bs ::: g(a))
}

object DList {

  def apply[A](f: List[A] => List[A]): DList[A] = new DList(f)

  def of[A](as: A*): DList[A] = DList[A](_ ++ as)

  def fromList[A](as: List[A]): DList[A] = DList[A](_ ++ as)

  def empty[A]: DList[A] = DList(identity)

  def singleton[A](a: A): DList[A] = DList(a :: _)

  def concat[A](xss: List[DList[A]]): DList[A] = xss.foldRight(empty[A])(_ ::: _)

  def fill[A](n: Int)(a: A): DList[A] = DList(xs => {
    @tailrec
    def go(m: Int)(result: List[A]): List[A] = {
      if (m <= 0) result
      else go(m - 1)(a :: result)
    }

    go(n)(xs)
  })
}

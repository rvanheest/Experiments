/*
  idea:
  - foldLeft as an Enumerator
  - acc together with f as Iteratee
 */

//sealed trait Enumerator[F] {
//  def apply[T](iteratee: Iteratee[F, T]): Iteratee[F, T]
//  def run[T](iteratee: Iteratee[F, T]): T
//}
//object Enumerator {
//  def apply[T](xs: T*): Enumerator[T]
//  def fromTextFile(filename: String): Enumerator[String]
//  def fromStream(s: InputStream, chunkSize: Int = 8192): Enumerator[Array[Byte]]
//}

sealed trait Input[+T]
case class Element[T](x: T) extends Input[T]
case object Empty extends Input[Nothing]
case object EOF extends Input[Nothing]

// first stab at an enumerator
def enum[F, T](xs: List[F])(iteratee: Iteratee[F, T]): Iteratee[F, T] = {
  (xs, iteratee) match {
    case (head :: tail, Continuation(k)) => enum(tail)(k(Element(head)))
    case _ => iteratee
  }
}

sealed trait Iteratee[F, T] {
  def run: T
}
case class Done[F, T](result: T, remainingInput: Input[F]) extends Iteratee[F, T] {
  override def run: T = result
}
case class Continuation[F, T](k: Input[F] => Iteratee[F, T]) extends Iteratee[F, T] {
  override def run: T = k(EOF) match {
    case Done(result, _) => result
    case Continuation(_) => sys.error("diverging iteratee")
    case Error(t) => throw t
  }
}
case class Error[F, T](throwable: Throwable) extends Iteratee[F, T] {
  override def run: T = throw throwable
}

def head[T]: Iteratee[T, T] = {
  def step: Input[T] => Iteratee[T, T] = {
    case Element(t) => Done(t, Empty)
    case Empty => Continuation(step)
    case EOF => Error(new NoSuchElementException)
  }

  Continuation(step)
}

enum("Hello World!".toList)(head).run

def length[T]: Iteratee[T, Int] = {
  def step(n: Int): Input[T] => Iteratee[T, Int] = {
    case EOF => Done(n, EOF)
    case Empty => Continuation(step(n))
    case Element(_) => Continuation(step(n + 1))
  }
  Continuation(step(0))
}

enum("Hello World!".toList)(length).run

package catsbook.monad

import cats.Monad

object CatsCustomMonad extends App {

  val optionMonad = new Monad[Option] {
    def pure[A](x: A): Option[A] = Some(x)

    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f

    def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(a1)) => tailRecM(a1)(f)
      case Some(Right(b)) => Some(b)
    }
  }
}

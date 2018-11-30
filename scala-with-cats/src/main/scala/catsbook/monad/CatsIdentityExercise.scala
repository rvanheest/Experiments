package catsbook.monad

import cats.{ Id, Monad }

import scala.annotation.tailrec
import scala.language.postfixOps

object CatsIdentityExercise extends App {

  val idMonad: Monad[Id] = new Monad[Id] {
    override def pure[A](a: A): Id[A] = a

    override def map[A, B](idA: Id[A])(f: A => B): Id[B] = f(idA)

    override def flatMap[A, B](idA: Id[A])(f: A => Id[B]): Id[B] = f(idA)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = {
      f(a) match {
        case Left(a2) => tailRecM(a2)(f)
        case Right(b) => pure(b)
      }
    }
  }

  println(idMonad.pure(123)) // 123
  println(idMonad.map(123)(2 *)) // 246
  println(idMonad.flatMap(123)(2 *)) // 246
}

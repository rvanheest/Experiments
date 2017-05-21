package experiments.cats.exercises

import cats._
import cats.implicits._

import scala.language.higherKinds

object monad extends App {

  println(Option(Option(1)).flatten)
  println(Option(None).flatten)
  println(List(List(1), List(2, 3)).flatten)
  println

  println(Monad[Option].pure(42))
  println

  println(Monad[List].flatMap(List(1, 2, 3))(x => List(x, x)))
  println

  println(List(1, 2, 3).flatMap(x => List(x, x)))
  println

  println(Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")))
  println(Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)))
  println

  case class OptionT[F[_], A](value: F[Option[A]])

  implicit def optionTMonad[F[_]](implicit F: Monad[F]): Monad[OptionT[F, ?]] = {
    new Monad[OptionT[F, ?]] {
      def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))

      def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = {
        OptionT {
          F.flatMap(fa.value) {
            case None => F.pure(None)
            case Some(a) => f(a).value
          }
        }
      }

      def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] = ???
    }
  }

  println(optionTMonad[List].pure(42))
}

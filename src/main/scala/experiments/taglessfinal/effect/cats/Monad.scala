package experiments.taglessfinal.effect.cats

import cats.Monad

import scala.language.higherKinds

object Monad {
  def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]

  implicit class MonadSyntax[F[_], A](val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit F: Monad[F]): F[B] = F.map(fa)(f)

    def flatMap[B](f: A => F[B])(implicit F: Monad[F]): F[B] = F.flatMap(fa)(f)
  }
}

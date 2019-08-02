package experiments.taglessfinal.effect.structuredMessages

import scala.language.higherKinds

trait Program[F[_]] {
  def finish[A](a: => A): F[A]

  def chain[A, B](fa: F[A], a2Fb: A => F[B]): F[B]

  def map[A, B](fa: F[A], a2b: A => B): F[B]
}
object Program {
  def apply[F[_]](implicit F: Program[F]): Program[F] = F

  implicit class ProgramSyntax[F[_], A](val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit F: Program[F]): F[B] = F.map(fa, f)

    def flatMap[B](f: A => F[B])(implicit F: Program[F]): F[B] = F.chain(fa, f)
  }

  def from[F[_], A](a: => A)(implicit F: Program[F]): F[A] = F.finish(a)
}

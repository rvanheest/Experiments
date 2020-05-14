package monadics.structures

import scala.language.higherKinds

trait MonadFail[M[_]] extends Monad[M] {

  def fail[A](s: String): M[A] = fail(new Exception(s))

  def fail[A](e: Throwable): M[A]
}

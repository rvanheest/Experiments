package monadics.structures

import scala.language.higherKinds

trait MonadTrans[T[_[_], _]] {

	def lift[M[+_], A](ma: M[A])(implicit monad: Monad[M]): T[M, A]
}

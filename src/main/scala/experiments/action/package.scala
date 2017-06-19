package experiments

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

package object action {

	case class CompositeException(throwables: Traversable[Throwable]) extends RuntimeException(throwables.map(_.getMessage).foldLeft("")((msg, t) => s"$msg\n$t"))

	implicit class TraversableTryExtensions[M[_], T](xs: M[Try[T]])(implicit ev: M[Try[T]] <:< Traversable[Try[T]]) {
		def collectResults(implicit canBuildFrom: CanBuildFrom[Nothing, T, M[T]]): Try[M[T]] = {
			if (xs.exists(_.isFailure))
				Failure(CompositeException(xs.flatMap {
					case Success(_) => Traversable.empty
					case Failure(CompositeException(ts)) => ts
					case Failure(e) => Traversable(e)
				}))
			else
				Success(xs.map(_.get).to(canBuildFrom))
		}
	}

	implicit class TryExtensions[T](val t: Try[T]) extends AnyVal {
		def doIfSuccess[A](f: T => A): Try[T] = {
			t match {
				case success @ Success(x) => Try {
					f(x)
					return success
				}
				case e => e
			}
		}
	}
}

package experiments

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.{ Failure, Success, Try }

package object action {

  case class PreconditionsFailedException(report: String, cause: Throwable = null) extends Exception(report)
  case class ActionRunFailedException(report: String, cause: Throwable = null) extends Exception(report)
  case class ActionException(row: Int, message: String, cause: Throwable = null) extends Exception(message, cause)
  case class CompositeException(throwables: Traversable[Throwable]) extends Exception(throwables.map(_.getMessage).foldLeft("")((msg, t) => s"$msg\n$t"))

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

  implicit class StringExtensions(val s: String) extends AnyVal {
    def isBlank: Boolean = {
      s match {
        case null | "" => true
        case _ => s.exists(!Character.isWhitespace(_))
      }
    }

    def toOption: Option[String] = {
      if (s.isBlank) Option.empty
      else Option(s)
    }
  }

  implicit class Action2Operators[A, T](val action: Action2[A, T]) extends AnyVal {
    def concat[S](other: Action2[T, S]): Action2[A, S] = new Action2[A, S] {
      private var pastSelf = false

      override def run(a: A): Try[S] = {
        pastSelf = false
        super.run(a)
      }

      override def checkPreconditions: Try[Unit] = {
        List(action, other).map(_.checkPreconditions).collectResults.map(_ => ())
      }

      override def execute(a: A): Try[S] = {
        for {
          t <- action.execute(a)
          _ = pastSelf = true
          s <- other.execute(t)
        } yield s
      }

      override def rollback(): Try[Unit] = {
        (if (pastSelf) List(other, action)
         else List(action))
          .map(_.rollback())
          .collectResults
          .map(_ => ())
      }
    }

    def first[S]: Action2[(A, S), (T, S)] = action split Action2.identity

    def second[S]: Action2[(S, A), (S, T)] = Action2.identity[S] split action

    def split[B, S](other: Action2[B, S]): Action2[(A, B), (T, S)] = new Action2[(A, B), (T, S)] {
      private var pastSelf = false

      override def run(a: (A, B)): Try[(T, S)] = {
        pastSelf = false
        super.run(a)
      }

      override def checkPreconditions: Try[Unit] = {
        List(action, other).map(_.checkPreconditions).collectResults.map(_ => ())
      }

      override def execute(tuple: (A, B)): Try[(T, S)] = {
        val (a, b) = tuple
        for {
          t <- action execute a
          _ = pastSelf = true
          s <- other execute b
        } yield t -> s
      }

      override def rollback(): Try[Unit] = {
        (if (pastSelf) List(other, action)
         else List(action))
          .map(_.rollback())
          .collectResults
          .map(_ => ())
      }
    }

    def fanout[S](other: Action2[A, S]): Action2[A, (T, S)] = new Action2[A, (T, S)] {
      private var pastSelf = false

      override def run(a: A): Try[(T, S)] = {
        pastSelf = false
        super.run(a)
      }

      override def checkPreconditions: Try[Unit] = {
        List(action, other).map(_.checkPreconditions).collectResults.map(_ => ())
      }

      override def execute(a: A): Try[(T, S)] = {
        for {
          t <- action execute a
          _ = pastSelf = true
          s <- other execute a
        } yield t -> s
      }

      override def rollback(): Try[Unit] = {
        (if (pastSelf) List(other, action)
         else List(action))
          .map(_.rollback())
          .collectResults
          .map(_ => ())
      }
    }

    def combine[S, R](other: Action2[A, S])(f: (T, S) => R): Action2[A, R] = {
      (action fanout other) concat Action2(a => Try { f.tupled(a) })
    }

    def map[S](f: T => S): Action2[A, S] = {
      action concat Action2(t => Try { f(t) })
    }

    def andThen[S](other: Action2[A, S]): Action2[A, S] = new Action2[A, S] {
      private var pastSelf = false

      override def run(a: A): Try[S] = {
        pastSelf = false
        super.run(a)
      }

      def checkPreconditions: Try[Unit] = {
        List(action, other).map(_.checkPreconditions).collectResults.map(_ => ())
      }

      def execute(a: A): Try[S] = {
        for {
          _ <- action execute a
          _ = pastSelf = true
          s <- other execute a
        } yield s
      }

      def rollback(): Try[Unit] = {
        (if (pastSelf) List(other, action)
         else List(action))
          .map(_.rollback())
          .collectResults
          .map(_ => ())
      }
    }

    def thenAnd[S](other: Action2[A, S]): Action2[A, T] = new Action2[A, T] {
      private var pastSelf = false

      override def run(a: A): Try[T] = {
        pastSelf = false
        super.run(a)
      }

      def checkPreconditions: Try[Unit] = {
        List(action, other).map(_.checkPreconditions).collectResults.map(_ => ())
      }

      def execute(a: A): Try[T] = {
        for {
          t <- action execute a
          _ = pastSelf = true
          _ <- other execute a
        } yield t
      }

      def rollback(): Try[Unit] = {
        (if (pastSelf) List(other, action)
         else List(action))
          .map(_.rollback())
          .collectResults
          .map(_ => ())
      }
    }
  }
}

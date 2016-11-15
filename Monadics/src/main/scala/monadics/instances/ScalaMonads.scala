package monadics

import monadics.structures.{Monad, MonadFail, MonadPlus, Semigroup}

import scala.language.reflectiveCalls
import scala.util.{Failure, Try}

package object ScalaMonads {
	implicit def optionIsMonadPlusAndMonadFail = new MonadPlus[Option] with MonadFail[Option] {
		def empty[A]: Option[A] = Option.empty

		def create[A](a: A): Option[A] = Option(a)

		def fail[A](e: Throwable): Option[A] = Option.empty

		override def map[A, B](option: Option[A])(f: A => B): Option[B] = {
			option.map(f)
		}

		def flatMap[A, B](option: Option[A])(f: A => Option[B]): Option[B] = {
			option.flatMap(f)
		}

		def orElse[A, B >: A](option1: Option[A], option2: => Option[B]): Option[B] = {
			option1.orElse(option2)
		}
	}

	implicit def tryIsMonadPlusAndMonadFail = new MonadPlus[Try] with MonadFail[Try] {
		def empty[A]: Try[A] = Failure(new NoSuchElementException("empty"))

		def create[A](a: A): Try[A] = Try(a)

		def fail[A](e: Throwable): Try[A] = Failure(e)

		override def map[A, B](functor: Try[A])(f: A => B): Try[B] = {
			functor.map(f)
		}

		def flatMap[A, B](monad: Try[A])(f: A => Try[B]): Try[B] = {
			monad.flatMap(f)
		}

		def orElse[A, B >: A](try1: Try[A], try2: => Try[B]): Try[B] = {
			try1.orElse(try2)
		}
	}

	implicit def eitherIsMonad[L] = new Monad[Either[L, ?]] {
		def create[R](a: R): Either[L, R] = Right(a)

		override def map[A, B](functor: Either[L, A])(f: A => B): Either[L, B] = {
			functor.right.map(f)
		}

		override def <*>[A, B](appFunc: Either[L, A => B], appA: Either[L, A]): Either[L, B] = {
			appFunc.right.flatMap(appA.right.map)
		}

		def flatMap[R, B](monad: Either[L, R])(f: R => Either[L, B]): Either[L, B] = {
			monad.right.flatMap(f)
		}
	}

	implicit def listIsMonadPlus = new MonadPlus[List] with MonadFail[List] {
		def empty[A]: List[A] = List.empty

		def create[A](a: A): List[A] = List(a)

		def fail[A](e: Throwable): List[A] = List.empty

		override def map[A, B](list: List[A])(f: A => B): List[B] = list.map(f)

		def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.flatMap(f)

		def orElse[A, B >: A](list1: List[A], list2: => List[B]): List[B] = list1 ++ list2
	}
}

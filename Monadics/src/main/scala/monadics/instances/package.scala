package monadics

import monadics.structures.{Monad, MonadPlus}

import scala.language.reflectiveCalls
import scala.util.{Failure, Try}

package object scalamonads {
	implicit def optionIsMonadPlus: MonadPlus[Option] = new MonadPlus[Option] {
		def empty[A]: Option[A] = Option.empty

		def create[A](a: A): Option[A] = Option(a)

		def fail[A](e: Throwable): Option[A] = Option.empty

		def map[A, B](option: Option[A])(f: A => B): Option[B] = {
			option.map(f)
		}

		def flatMap[A, B](option: Option[A])(f: A => Option[B]): Option[B] = {
			option.flatMap(f)
		}

		def orElse[A, B >: A](option1: Option[A], option2: => Option[B]): Option[B] = {
			option1.orElse(option2)
		}
	}

	implicit def tryIsMonadPlus: Monad[Try] = new Monad[Try] {
		def create[A](a: A): Try[A] = Try(a)

		def fail[A](e: Throwable): Try[A] = Failure(e)

		def map[A, B](functor: Try[A])(f: A => B): Try[B] = {
			functor.map(f)
		}

		def flatMap[A, B](monad: Try[A])(f: A => Try[B]): Try[B] = {
			monad.flatMap(f)
		}
	}

	implicit def listIsMonadPlus: MonadPlus[List] = new MonadPlus[List] {
		def empty[A]: List[A] = List.empty

		def create[A](a: A): List[A] = List(a)

		def fail[A](e: Throwable): List[A] = throw e

		def map[A, B](list: List[A])(f: A => B): List[B] = list.map(f)

		def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.flatMap(f)

		def orElse[A, B >: A](list1: List[A], list2: => List[B]): List[B] = list1 ++ list2
	}

	type FunctionMonad[S] = Monad[({type s[x] = S => x})#s]

	implicit def functionIsMonadPlus[S]: FunctionMonad[S] = new FunctionMonad[S] {
		def create[A](a: A): Function[S, A] = {
			_ => a
		}

		def fail[A](e: Throwable): Function[S, A] = throw e

		def map[A, B](functor: Function[S, A])(f: A => B): Function[S, B] = {
			f.compose(functor)
		}

		def flatMap[A, B](monad: Function[S, A])(f: A => Function[S, B]): Function[S, B] = {
			s => f(monad(s))(s)
		}
	}
}

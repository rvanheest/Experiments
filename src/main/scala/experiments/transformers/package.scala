package experiments

import experiments.monadics.{Monad, MonadPlus}

import scala.util.Try

package object transformers {

	implicit object ListMonad extends MonadPlus[List] {
		def empty[A]: List[A] = Nil

		def create[A](a: A): List[A] = a :: Nil

		def map[A, B](list: List[A])(f: (A) => B): List[B] = list map f

		def flatMap[A, B](list: List[A])(f: (A) => List[B]): List[B] = list flatMap f

		def getOrElse[A, B >: A](alt: List[A], default: => B): B = ???

		def orElse[A, B >: A](list1: List[A], list2: => List[B]): List[B] = list1 ++ list2
	}

	implicit object OptionMonad extends MonadPlus[Option] {
		def empty[A]: Option[A] = None

		def create[A](a: A): Option[A] = Option(a)

		def map[A, B](option: Option[A])(f: (A) => B): Option[B] = option map f

		def flatMap[A, B](option: Option[A])(f: (A) => Option[B]): Option[B] = option flatMap f

		def getOrElse[A, B >: A](option: Option[A], default: => B): B = option getOrElse default

		def orElse[A, B >: A](option1: Option[A], option2: => Option[B]): Option[B] = option1 orElse option2
	}

	implicit object TryMonad extends Monad[Try] {
		def create[A](a: A): Try[A] = Try(a)

		def map[A, B](t: Try[A])(f: (A) => B): Try[B] = t map f

		def flatMap[A, B](t: Try[A])(f: (A) => Try[B]): Try[B] = t flatMap f
	}
}

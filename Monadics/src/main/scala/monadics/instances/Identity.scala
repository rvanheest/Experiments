package monadics.instances

import monadics.structures.{ Comonad, Equals, Monad }

class Identity[A](id: => A)(implicit monad: Monad[Identity] with Comonad[Identity]) {

	def run: A = id

	def map[B](f: A => B): Identity[B] = monad.map(this)(f)

	def as[B](b: => B): Identity[B] = monad.as(this, b)

	def void: Identity[Unit] = monad.void(this)

	def zipWith[B](f: A => B): Identity[(A, B)] = monad.zipWith(this)(f)

	def <*>[B, C](other: Identity[B])(implicit ev: Identity[A] <:< Identity[B => C]): Identity[C] = monad.<*>(this, other)

	def *>[B](other: Identity[B]): Identity[B] = monad.*>(this, other)

	def <*[B](other: Identity[B]): Identity[A] = monad.<*(this, other)

	def <**>[B](other: Identity[A => B]): Identity[B] = monad.<**>(this, other)

	def flatMap[B](f: A => Identity[B]): Identity[B] = monad.flatMap(this)(f)

	def andThen[B](other: Identity[B]): Identity[B] = monad.andThen(this, other)

	def thenAnd[B](other: Identity[B]): Identity[A] = monad.thenAnd(this, other)

	def flatten[B](implicit ev: A <:< Identity[B]): Identity[B] = monad.flatten(this)
}

object Identity {

	def apply[A](a: => A): Identity[A] = {
		new Identity(a)
	}

	implicit def identityIsEquals[A](implicit aEquals: Equals[A]): Equals[Identity[A]] = {
		Equals.create((id1, id2) => aEquals.equals(id1.run, id2.run))
	}

	implicit def identityIsMonad: Monad[Identity] with Comonad[Identity] = new Monad[Identity] with Comonad[Identity] {

		def create[A](a: A): Identity[A] = Identity(a)

		override def map[A, B](identity: Identity[A])(f: A => B): Identity[B] = {
			Identity(f(identity.run))
		}

		def flatMap[A, B](identity: Identity[A])(f: A => Identity[B]): Identity[B] = {
			Identity(f(identity.run).run)
		}

		def extract[A](identity: Identity[A]): A = identity.run

		def extend[A, B](identity: Identity[A])(f: Identity[A] => B): Identity[B] = {
			Identity[B](f(identity))
		}
	}
}

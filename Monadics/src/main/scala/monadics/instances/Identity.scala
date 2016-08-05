package monadics.instances

import monadics.structures.Monad

case class Identity[A](id: A)(implicit monad: Monad[Identity]) {

	def map[B](f: A => B): Identity[B] = monad.map(this)(f)

	def <*>[B, C](other: Identity[B])(implicit ev: A <:< (B => C)): Identity[C] = {
		monad.<*>(this.map(ev), other)
	}

	def *>[B](other: Identity[B]): Identity[B] = monad.*>(this, other)

	def <*[B](other: Identity[B]): Identity[A] = monad.<*(this, other)

	def <**>[B](other: Identity[A => B]): Identity[B] = monad.<**>(this, other)

	def flatMap[B](f: A => Identity[B]): Identity[B] = monad.flatMap(this)(f)

	def andThen[B](other: Identity[B]): Identity[B] = monad.andThen(this, other)

	def thenAnd[B](other: Identity[B]): Identity[A] = monad.thenAnd(this, other)

	def flatten[B](implicit ev: A <:< Identity[B]): Identity[B] = monad.flatten(this)(ev)
}

package object identityMonad {
	implicit def identityIsMonad = new Monad[Identity] {
		def create[A](a: A): Identity[A] = Identity(a)

		def map[A, B](identity: Identity[A])(f: A => B): Identity[B] = {
			Identity(f(identity.id))
		}

		def flatMap[A, B](identity: Identity[A])(f: A => Identity[B]): Identity[B] = {
			f(identity.id)
		}
	}
}

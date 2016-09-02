package monadics.instances

import monadics.instances.Identity.IdentityMonad
import monadics.structures.Monad

class Identity[A](id: A)(implicit monad: IdentityMonad) {

	def run: A = id

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

object Identity {
	type IdentityMonad = Monad[Identity]

	def apply[A](a: A)(implicit monad: IdentityMonad): Identity[A] = {
		new Identity(a)
	}

	implicit def identityIsMonad = new IdentityMonad { self =>

		def create[A](a: A): Identity[A] = new Identity(a)(self)

		def fail[A](e: Throwable): Identity[A] = throw e

		def map[A, B](identity: Identity[A])(f: A => B): Identity[B] = {
			new Identity(f(identity.run))(self)
		}

		def flatMap[A, B](identity: Identity[A])(f: A => Identity[B]): Identity[B] = {
			f(identity.run)
		}
	}
}

package monadics.instances

import monadics.structures.Monad

case class Identity[A](private val id: A)(implicit monad: Monad[Identity]) {

	def run: A = id

	def map[B](f: A => B): Identity[B] = monad.map(this)(f)

	def <*>[B, C](other: Identity[B])(implicit ev: Identity[A] <:< Identity[(B => C)]): Identity[C] = monad.<*>(this, other)

	def *>[B](other: Identity[B]): Identity[B] = monad.*>(this, other)

	def <*[B](other: Identity[B]): Identity[A] = monad.<*(this, other)

	def <**>[B](other: Identity[A => B]): Identity[B] = monad.<**>(this, other)

	def flatMap[B](f: A => Identity[B]): Identity[B] = monad.flatMap(this)(f)

	def andThen[B](other: Identity[B]): Identity[B] = monad.andThen(this, other)

	def thenAnd[B](other: Identity[B]): Identity[A] = monad.thenAnd(this, other)

	def flatten[B](implicit ev: A <:< Identity[B]): Identity[B] = monad.flatten(this)(ev)

//	override def equals(obj: scala.Any) = {
//		obj match {
//			case that: Identity[A] => this.run == that.run
//			case _ => false
//		}
//	}
//
//	override def toString = s"Identity($id)"
}

object Identity {

//	def apply[A](a: A)(implicit monad: Monad[Identity]): Identity[A] = {
//		new Identity(a)
//	}

	implicit def identityIsMonad = new Monad[Identity] { self =>

		def create[A](a: A): Identity[A] = new Identity(a)(self)

		override def map[A, B](identity: Identity[A])(f: A => B): Identity[B] = {
			new Identity(f(identity.run))(self)
		}

		def flatMap[A, B](identity: Identity[A])(f: A => Identity[B]): Identity[B] = {
			f(identity.run)
		}
	}
}

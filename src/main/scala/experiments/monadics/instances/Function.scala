package experiments.monadics.instances

import experiments.monadics.{Arrow, Monad}

import scala.language.reflectiveCalls

case class Function[A, B](f: A => B)(implicit arrow: Arrow[Function], monad: Monad[({ type s[x] = Function[A, x] })#s]) {

	def apply(a: A): B = f(a)

	def andThen[C](g: Function[B, C]): Function[A, C] = arrow.compose(g, this)

	def compose[C](g: Function[C, A]): Function[C, B] = arrow.compose(this, g)

	def first[C]: Function[(A, C), (B, C)] = arrow.first(this)

	def second[C]: Function[(C, A), (C, B)] = arrow.second(this)

	def ***[C, D](other: Function[C, D]): Function[(A, C), (B, D)] = arrow.***(this, other)

	def &&&[C](other: Function[A, C]): Function[A, (B, C)] = arrow.&&&(this, other)

	def liftA2[C, D](other: Function[A, C])(f: (B, C) => D): Function[A, D] = arrow.liftA2(this, other)(f)

	def map[C](g: B => C): Function[A, C] = monad.map(this)(g)

	def <*>[C, D](other: Function[A, C])(implicit ev: B <:< (C => D)): Function[A, D] = {
		monad.<*>(this.map(ev), other)
	}

	def *>[C](other: Function[A, C]): Function[A, C] = {
		monad.*>(this, other)
	}

	def <*[C](other: Function[A, C]): Function[A, B] = {
		monad.<*(this, other)
	}

	def <**>[C](other: Function[A, B => C]): Function[A, C] = {
		monad.<**>(this, other)
	}

	def flatMap[C](f: B => Function[A, C]): Function[A, C] = {
		monad.flatMap(this)(f)
	}
}
object Function {
	def identity[A](implicit arrow: Arrow[Function]): Function[A, A] = arrow.id[A]

	def create[A, B](f: A => B)(implicit arrow: Arrow[Function]): Function[A, B] = arrow.create(f)
}

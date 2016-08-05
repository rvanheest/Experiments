package monadics.instances

import monadics.instances.stateMonad.StateMonad
import monadics.structures.Monad

sealed abstract class Tree[A](implicit monad: Monad[Tree]) {
	def map[B](f: A => B): Tree[B] = monad.map(this)(f)

	def flatMap[B](f: A => Tree[B]): Tree[B] = monad.flatMap(this)(f)

	def zipTree[B](other: Tree[B]): Option[Tree[(A, B)]]

	def number(implicit monad: StateMonad[Int]): State[Int, Tree[Int]]
}

case class Leaf[A](a: A)(implicit monad: Monad[Tree]) extends Tree[A] {
	def zipTree[B](other: Tree[B]): Option[Tree[(A, B)]] = {
		other match {
			case Leaf(b: B) => Option(Leaf(a, b))
			case _ => Option.empty
		}
	}

	def number(implicit stateMonad: StateMonad[Int]): State[Int, Tree[Int]] = {
		State.get[Int].map(n => Leaf(n + 1))
	}
}

case class Branch[A](left: Tree[A], right: Tree[A])(implicit monad: Monad[Tree]) extends Tree[A] {
	def zipTree[B](other: Tree[B]): Option[Tree[(A, B)]] = {
		other match {
			case Branch(l, r) =>
				for {
					ll <- left.zipTree(l)
					rr <- right.zipTree(r)
				} yield Branch(ll, rr)
		}
	}

	def number(implicit stateMonad: StateMonad[Int]): State[Int, Tree[Int]] = {
		for {
			l <- left.number
			r <- right.number
		} yield Branch(l, r)
	}
}

package object treeMonad {
	implicit def treeIsMonad: Monad[Tree] = new Monad[Tree] {
		def create[A](a: A) = Leaf(a)

		def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
			tree match {
				case Leaf(a) => Leaf(f(a))
				case Branch(l, r) => Branch[B](map(l)(f), map(r)(f))
			}
		}

		def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] = {
			tree match {
				case Leaf(a) => f(a)
				case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
			}
		}
	}
}

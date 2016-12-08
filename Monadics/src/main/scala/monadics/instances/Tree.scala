package monadics.instances

import monadics.structures.{Equals, Monad}

import scala.annotation.tailrec

sealed abstract class Tree[A](implicit monad: Monad[Tree]) {
	def map[B](f: A => B): Tree[B] = monad.map(this)(f)

	def flatMap[B](f: A => Tree[B]): Tree[B] = monad.flatMap(this)(f)

	def zipTree[B](other: Tree[B]): Option[Tree[(A, B)]]

	def number(implicit monad: Monad[State[Int, ?]]): State[Int, Tree[Int]]
}

object Tree {

	implicit def treeIsEquals[A](implicit aEquals: Equals[A]): Equals[Tree[A]] = new Equals[Tree[A]] {
		def equals(x: Tree[A], y: Tree[A]) = {

			@tailrec
			def rec(todo: List[(Tree[A], Tree[A])], acc: Boolean = true): Boolean = {
				if (acc)
					todo match {
						case Nil => acc
						case (Leaf(a), Leaf(b)) :: tail => rec(tail, acc && aEquals.equals(a, b))
						case (Branch(l1, r1), Branch(l2, r2)) :: tail => rec((l1, l2) :: (r1, r2) :: tail, acc)
						case _ => false
					}
				else
					false
			}

			rec(List((x, y)))
		}
	}

	implicit val treeIsMonad: Monad[Tree] = new Monad[Tree] {
		def create[A](a: A) = Leaf(a)

		override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
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

case class Leaf[A](a: A)(implicit monad: Monad[Tree]) extends Tree[A] {
	def zipTree[B](other: Tree[B]): Option[Tree[(A, B)]] = {
		other match {
			case Leaf(b) => Option(Leaf(a, b))
			case _ => Option.empty
		}
	}

	def number(implicit stateMonad: Monad[State[Int, ?]]): State[Int, Tree[Int]] = {
		def tick = new State[Int, Int](s => (s, s + 1))

		tick.map(Leaf(_))
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
			case _ => Option.empty
		}
	}

	def number(implicit stateMonad: Monad[State[Int, ?]]): State[Int, Tree[Int]] = {
		for {
			l <- left.number
			r <- right.number
		} yield Branch(l, r)
	}
}

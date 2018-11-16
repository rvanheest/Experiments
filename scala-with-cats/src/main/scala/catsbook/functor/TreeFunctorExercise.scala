package catsbook.functor

import cats.Functor
import cats.Show
import cats.instances.int._
import cats.syntax.functor._
import cats.syntax.show._

object TreeFunctorExercise extends App {

  sealed trait Tree[+A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  implicit class TreeShow[A](implicit showA: Show[A]) extends Show[Tree[A]] {
    implicit val self: Show[Tree[A]] = this

    override def show(tree: Tree[A]): String = tree match {
      case Leaf(value) => value.show
      case Branch(left, right) => s"[${ left.show }, ${ right.show }]"
    }
  }

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }
    }
  }

  val tree: Tree[Int] = Branch(
    Leaf(1),
    Branch(
      Leaf(2),
      Leaf(3),
    ),
  )
  println(tree.show) // [1, [2, 3]]
  val tree2 = tree.map(_ + 1)
  println(tree2.show) // [2, [3, 4]]
}

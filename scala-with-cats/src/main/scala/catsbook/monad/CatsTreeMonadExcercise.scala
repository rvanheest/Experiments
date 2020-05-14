package catsbook.monad

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._

object CatsTreeMonadExcercise extends App {

  sealed trait Tree[+A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    def pure[A](value: A): Tree[A] = leaf(value)

    def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] = {
      tree match {
        case Leaf(value) => f(value)
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      }
    }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      flatMap(f(a)) {
        case Left(value) => tailRecM(value)(f)
        case Right(value) => Leaf(value)
      }
    }
  }

  println {
    for {
      tree1 <- branch(leaf(100), leaf(200))
      tree2 <- branch(leaf(tree1 - 10), leaf(tree1 + 10))
      tree3 <- branch(leaf(tree2 - 1), leaf(tree2 + 1))
    } yield tree3
  } /*
     * Branch(
     *   Branch(
     *     Branch(
     *       Leaf(89),
     *       Leaf(91)
     *     ),
     *     Branch(
     *       Leaf(109),
     *       Leaf(111)
     *     )
     *   ),
     *   Branch(
     *     Branch(
     *       Leaf(189),
     *       Leaf(191)
     *     ),
     *     Branch(
     *       Leaf(209),
     *       Leaf(211)
     *     )
     *   )
     * )
     */
}

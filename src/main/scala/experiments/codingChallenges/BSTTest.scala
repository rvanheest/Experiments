package experiments.codingChallenges

import scala.annotation.tailrec

case class BST private() {

  var root: Node = _

  def isEmpty: Boolean = root == null

  def add(element: Int): Unit = {
    if (isEmpty)
      root = Node(element)
    else
      root.add(element)
  }

  def contains(element: Int): Boolean = {
    !isEmpty && root.contains(element)
  }

  def traverseDeptFirstPreOrder: Stream[Int] = {
    if (isEmpty)
      Stream.empty
    else
      root.traverseDeptFirstPreOrder
  }

  def traverseDeptFirstInOrder: Stream[Int] = {
    if (isEmpty)
      Stream.empty
    else
      root.traverseDeptFirstInOrder
  }

  def traverseDeptFirstPostOrder: Stream[Int] = {
    if (isEmpty)
      Stream.empty
    else
      root.traverseDeptFirstPostOrder
  }

  def traverseBreadthFirst: List[Int] = {
    if (isEmpty)
      List.empty
    else
      root.traverseBreadthFirst
  }

  case class Node(value: Int, var left: Node = null, var right: Node = null) {

    def add(v: Int): Unit = {
      @tailrec
      def tailrecursion(bst: Node): Unit = {
        if (v <= value) {
          if (this.left == null)
            this.left = Node(v)
          else
            tailrecursion(left)
        }
        else {
          if (this.right == null)
            this.right = Node(v)
          else
            tailrecursion(right)
        }
      }

      tailrecursion(this)
    }

    def contains(v: Int): Boolean = {
      @tailrec
      def tailrecursion(bst: Node): Boolean = {
        if (v == bst.value)
          true
        else if (v < bst.value) {
          bst.left != null && tailrecursion(bst.left)
        }
        else {
          bst.right != null && tailrecursion(bst.right)
        }
      }

      tailrecursion(this)
    }

    def traverseDeptFirstPreOrder: Stream[Int] = {
      value #:: Stream(left, right).filterNot(_ == null).flatMap(_.traverseDeptFirstPreOrder)
    }

    def traverseDeptFirstInOrder: Stream[Int] = {
      val head = Stream(value)
      val l = Stream(left).filterNot(_ == null).flatMap(_.traverseDeptFirstInOrder)
      val r = Stream(right).filterNot(_ == null).flatMap(_.traverseDeptFirstInOrder)

      l #::: head #::: r
    }

    def traverseDeptFirstPostOrder: Stream[Int] = {
      val head = Stream(value)
      val l = Stream(left).filterNot(_ == null).flatMap(_.traverseDeptFirstPostOrder)
      val r = Stream(right).filterNot(_ == null).flatMap(_.traverseDeptFirstPostOrder)

      l #::: r #::: head
    }

    def traverseBreadthFirst: List[Int] = {
      def tbf(bsts: List[Node]): List[Int] = {
        bsts match {
          case Nil => Nil
          case xs => xs.map(_.value) ::: tbf(xs.flatMap(bst => List(bst.left, bst.right).filterNot(_ == null)))
        }
      }

      tbf(this :: Nil)
    }
  }
}

object BST {

  def empty: BST = new BST

  def from(values: Int*): BST = {
    val tree = empty
    values.foreach(tree.add)
    tree
  }
}

object BSTTest extends App {

  val tree = BST.from(5, 3, 7, 1, 2, 4, 6, 8)

  println(tree)

  println(tree.contains(2))
  println(tree.contains(0))
  println(tree.contains(10))

  println(tree.traverseDeptFirstPreOrder.mkString("[", ", ", "]"))
  println(tree.traverseDeptFirstInOrder.mkString("[", ", ", "]"))
  println(tree.traverseDeptFirstPostOrder.mkString("[", ", ", "]"))
  println(tree.traverseBreadthFirst.mkString("[", ", ", "]"))

  /*
  BST(5,
  | BST(3,
  | | BST(1,
  | | | null,
  | | | BST(2,null,null)
  | | ),
  | | BST(4,null,null)
  | ),
  | BST(7,
  | | BST(6,null,null),
  | | BST(8,null,null)
  | )
  )
   */
}

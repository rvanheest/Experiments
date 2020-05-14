package catsbook.foldable

object FoldableExamples extends App {

  def show[A](list: List[A]): String = {
    list.foldRight("nil")((item, acc) => s"$item then $acc")
  }

  def showReverse[A](list: List[A]): String = {
    list.foldLeft("nil")((acc, item) => s"$item then $acc")
  }

  println(show(List.empty)) // "nil"
  println(show(List(1, 2, 3))) // "1 then 2 then 3 then nil"

  println(showReverse(List.empty)) // "nil"
  println(showReverse(List(1, 2, 3))) // "3 then 2 then 1 then nil"


}

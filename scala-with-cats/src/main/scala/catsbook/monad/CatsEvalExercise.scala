package catsbook.monad

import cats.Eval

object CatsEvalExercise extends App {

  def foldRightEval[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = {
    as match {
      case Nil => Eval.now(acc)
      case head :: tail => Eval.defer { foldRightEval(tail, acc)(fn).map(fn(head, _)) }
    }
  }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = foldRightEval(as, acc)(fn).value

  println(foldRight((1L to 100000L).toList, 0L)(_ + _))
}

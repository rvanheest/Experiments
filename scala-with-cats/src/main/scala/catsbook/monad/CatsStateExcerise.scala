package catsbook.monad

import cats.data.State
import cats.syntax.applicative._
import cats.syntax.flatMap._

object CatsStateExcerise extends App {

  type CalcState[A] = State[List[Int], A]
  val CalcState = State

  def evalOne(sym: String): CalcState[Int] = {
    sym match {
      case "+" => evaluate(_ + _)
      case "-" => evaluate(_ - _)
      case "*" => evaluate(_ * _)
      case "/" => evaluate(_ / _)
      case _ => CalcState(stack => {
        val num = sym.toInt
        (num :: stack, num)
      })
    }
  }

  private def evaluate(f: (Int, Int) => Int): CalcState[Int] = CalcState {
    case x :: y :: tail =>
      val result = f(x, y)
      (result :: tail, result)
    case _ => sys.error("FAIL!!!")
  }

  println(evalOne("42").run(Nil).value) // (List(42), 42)

  val programEvalOne = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    result <- evalOne("+")
  } yield result
  println(programEvalOne.run(Nil).value) // (List(3), 3)

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState])(_ >> evalOne(_))
  }

  println(evalAll(List("1", "2", "+", "3", "*")).run(Nil).value) // (List(9), 9)

  val programEvalAll = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    result <- evalOne("*")
  } yield result
  println(programEvalAll.run(Nil).value) // (List(21), 21)

  def evalInput(input: String): Int = {
    val inputList = input.split(" ").toList
    evalAll(inputList).runA(Nil).value
  }

  println(evalInput("1 2 + 3 *")) // 9
  println(evalInput("1 2 + 3 4 + *")) // 21
}

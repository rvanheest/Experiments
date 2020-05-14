package experiments.monadics.tests

import java.util.NoSuchElementException

import experiments.monadics.instances.State

object StateTest extends App {

  type Stack = List[Int]

  def pop: State[Stack, Int] = new State({
    case x :: xs => (x, xs)
    case Nil => throw new NoSuchElementException("the stack is empty")
  })

  def push(x: Int): State[Stack, Unit] = new State(xs => ((), x :: xs))

  def stackManip: State[Stack, Int] = {
    for {
      _ <- push(3)
      _ <- pop
      x <- pop
    } yield x
  }

  println(stackManip.run(List(5, 8, 2, 1)))
}

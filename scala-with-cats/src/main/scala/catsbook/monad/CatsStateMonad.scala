package catsbook.monad

import cats.data.State

object CatsStateMonad extends App {

  val a = State[Int, String](state => (state, s"The state is $state"))
  println(a.run(10).value) // (10, "The state is 10")
  println(a.runS(10).value) // 10
  println(a.runA(10).value) // "The state is 10"

  val step1 = State[Int, String](num => {
    val result = num + 1
    (result, s"Result of step1: $result")
  })
  val step2 = State[Int, String](num => {
    val result = num * 2
    (result, s"Result of step2: $result")
  })
  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)
  println(both.run(20).value) // (42, ("Result of step1: 21", "Result of step2: 42"))

  val getDemo = State.get[Int]
  println(getDemo.run(10).value) // (10, 10)

  val setDemo = State.set(30)
  println(setDemo.run(10).value) // (30, ())

  val pureDemo = State.pure[Int, String]("Result")
  println(pureDemo.run(10).value) // (10, "Result")

  val inspectDemo = State.inspect[Int, String](_ + "!")
  println(inspectDemo.run(10).value) // (10, "10!")

  val modifyDemo = State.modify[Int](_ + 1)
  println(modifyDemo.run(10).value) // (11, ())

  val program = for {
    a <- State.get[Int]
    _ <- State.set(a + 1)
    b <- State.get[Int]
    _ <- State.modify[Int](_ + 1)
    c <- State.inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  println(program.run(1).value) // (3, (1, 2, 3000))
}

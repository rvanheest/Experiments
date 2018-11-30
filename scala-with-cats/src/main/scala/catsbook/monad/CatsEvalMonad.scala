package catsbook.monad

import cats.Eval

object CatsEvalMonad extends App {

  val now = Eval.now(math.random() + 1000) // similar to val, should give the same result each time
  val later = Eval.later(math.random() + 2000) // similar to lazy val, should give the same result each time
  val always = Eval.always(math.random() + 3000) // similar to def, should give different results each time

  println(now.value)
  println(now.value)
  println(later.value)
  println(later.value)
  println(always.value)
  println(always.value)
  println

  val greetings = Eval.always { println("Step 1"); "Hello" }
    .map(str => { println("Step 2"); s"$str world" })

  println(greetings.value)
  println(greetings.value)
  println

  val result = for {
    a <- Eval.now { println("calculating A"); 40 }
    b <- Eval.always { println("calculating B"); 2 }
  } yield {
    println("adding A and B")
    a + b
  }
  println("------") // separator to show how `result` is evaluated

  println(result.value)
  println(result.value)
  println

  val saying = Eval.always { println("Step 1"); "The cat" }
    .map(str => { println("Step 2"); s"$str sat on" })
    .memoize
    .map(str => { println("Step 3"); s"$str the mat" })

  println(saying.value)
  println(saying.value)
  println

  def factorial1(n: BigInt): BigInt = {
    if (n == 1) n
    else n * factorial1(n - 1)
  }

  // factorial1(50000) -> StackOverflowError

  def factorial2(n: BigInt): Eval[BigInt] = {
    if (n == 1) Eval.now(n)
    else factorial2(n - 1).map(_ * n)
  }

  // factorial2(50000) -> StackOverflowError

  def factorial3(n: BigInt): Eval[BigInt] = {
    if (n == 1) Eval.now(n)
    else Eval.defer { factorial3(n - 1).map(_ * n) }
  }

  println(factorial3(50000).value)
}

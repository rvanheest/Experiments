package experiments.taglessfinal.effect.cats

import cats.data.State

object Testing extends App {

  case class TestData(input: List[String], output: List[ConsoleOut], randoms: List[Int])

  type TestIO[A] = State[TestData, A]
  val TestIO = State

  implicit val consoleTestIO: Console[TestIO] = new Console[TestIO] {
    override def println(msg: ConsoleOut): TestIO[Unit] = {
      for {
        testData <- TestIO.get[TestData]
        setted <- TestIO.set(testData.copy(output = msg :: testData.output))
      } yield setted
    }

    override def readLine(): TestIO[String] = TestIO(testData => {
      testData.input match {
        case Nil => throw new Exception("empty input")
        case input :: inputs => (testData.copy(input = inputs), input)
      }
    })
  }

  implicit val randomTestIO: Random[TestIO] = new Random[TestIO] {
    override def nextInt(upper: Int): TestIO[Int] = TestIO(testData => {
      testData.randoms match {
        case Nil => throw new Exception("no random numbers available")
        case random :: randoms => (testData.copy(randoms = randoms), random)
      }
    })
  }

  val testData = TestData(
    input = List("Richard", "4", "y", "3", "n"),
    output = List(),
    randoms = List(1, 2)
  )
  val TestData(inputResult, outputResult, randomsResult) = Game.runApplication[TestIO].runS(testData).value
  println("input shouldBe empty --> " + inputResult.isEmpty)
  println("randoms shouldBe empty --> " + randomsResult.isEmpty)
  println("output should contain all printlns --> " + outputResult.reverse.map(s => s"  * $s --> ${ s.en }").mkString("[\n", ",\n", "\n]"))
}

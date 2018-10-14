package experiments.effect.abstraction

object Testing extends App {

  case class TestData(input: List[String], output: List[String], randoms: List[Int])

  type TestIO[A] = State[TestData, A]
  val TestIO = State

  implicit val programTestIO: Program[TestIO] = new Program[TestIO] {
    override def finish[A](a: => A): TestIO[A] = TestIO.from(a)

    override def chain[A, B](fa: TestIO[A], a2Fb: A => TestIO[B]): TestIO[B] = fa.flatMap(a2Fb)

    override def map[A, B](fa: TestIO[A], a2b: A => B): TestIO[B] = fa.map(a2b)
  }

  implicit val consoleTestIO: Console[TestIO] = new Console[TestIO] {
    override def println(s: String): TestIO[Unit] = TestIO(testData => {
      ((), testData.copy(output = s :: testData.output))
    })

    override def readLine(): TestIO[String] = TestIO(testData => {
      testData.input match {
        case Nil => throw new Exception("empty input")
        case input :: inputs => (input, testData.copy(input = inputs))
      }
    })
  }

  implicit val randomTestIO: Random[TestIO] = new Random[TestIO] {
    override def nextInt(upper: Int): TestIO[Int] = TestIO(testData => {
      testData.randoms match {
        case Nil => throw new Exception("no random numbers available")
        case random :: randoms => (random, testData.copy(randoms = randoms))
      }
    })
  }

  val testData = TestData(
    input = List("Richard", "4", "y", "3", "n"),
    output = List(),
    randoms = List(1, 2)
  )
  val TestData(inputResult, outputResult, randomsResult) = Game.runApplication[TestIO].execute(testData)
  println("input shouldBe empty --> " + inputResult.isEmpty)
  println("randoms shouldBe empty --> " + randomsResult.isEmpty)
  println("output should contain all printlns --> " + outputResult.reverse.map(s => s"  * $s").mkString("[\n", ",\n", "\n]"))
}

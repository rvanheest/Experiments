package experiments.taglessfinal.effect.structuredMessages

import scala.io.StdIn

object Main extends App {

  implicit val programIO: Program[IO] = new Program[IO] {
    override def finish[A](a: => A): IO[A] = IO.from(a)

    override def chain[A, B](fa: IO[A], a2Fb: A => IO[B]): IO[B] = fa.flatMap(a2Fb)

    override def map[A, B](fa: IO[A], a2b: A => B): IO[B] = fa.map(a2b)
  }

  implicit val consoleIO: Console[IO] = new Console[IO] {
    override def println(msg: ConsoleOut): IO[Unit] = IO(() => scala.Console.println(msg.en))

    override def readLine(): IO[String] = IO(() => StdIn.readLine())
  }

  implicit val randomIO: Random[IO] = new Random[IO] {
    override def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(5))
  }

  Game.runApplication[IO].run()
}

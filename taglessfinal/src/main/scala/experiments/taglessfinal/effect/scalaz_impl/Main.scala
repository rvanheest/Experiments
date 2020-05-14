package experiments.taglessfinal.effect.scalaz_impl

import scalaz.Monad

import scala.io.StdIn

object Main extends App {

  implicit val monadIO: Monad[IO] = new Monad[IO] {
    override def point[A](a: => A): IO[A] = IO.from(a)

    override def map[A, B](ioA: IO[A])(f: A => B): IO[B] = ioA map f

    override def bind[A, B](ioA: IO[A])(f: A => IO[B]): IO[B] = ioA flatMap f
  }

  implicit val consoleIO: Console[IO] = new Console[IO] {
    override def println(msg: ConsoleOut): IO[Unit] = IO(() => scala.Console.println(msg.en))

    override def readLine(): IO[String] = IO(() => StdIn.readLine())
  }

  implicit val randomIO: Random[IO] = new Random[IO] {
    override def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))
  }

  Game.runApplication[IO].run()
}

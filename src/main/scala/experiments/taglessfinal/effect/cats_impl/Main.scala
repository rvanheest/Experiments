package experiments.taglessfinal.effect.cats_impl

import cats.Monad

import scala.io.StdIn

object Main extends App {

  implicit val monadIO: Monad[IO] = new Monad[IO] {
    override def map[A, B](ioA: IO[A])(f: A => B): IO[B] = ioA map f

    override def flatMap[A, B](ioA: IO[A])(f: A => IO[B]): IO[B] = ioA flatMap f

    override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = IO(() => {
      f(a).run().fold(a => tailRecM(a)(f).run(), b => b)
    })

    override def pure[A](a: A): IO[A] = IO.from(a)
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

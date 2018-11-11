package experiments.taglessfinal.weather.cats_impl

import java.util.concurrent.atomic.AtomicReference

import cats.data.EitherT
import cats.effect.IO
import cats.mtl.{ DefaultApplicativeAsk, MonadState }
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{ Applicative, Functor, Monad }
import experiments.taglessfinal.weather.cats_impl.City.ErrorHandler
import experiments.taglessfinal.weather.cats_impl.Config.ConfigAsk
import experiments.taglessfinal.weather.cats_impl.Requests.{ Requests, RequestsState }

import scala.language.higherKinds

object Main extends App {

  val config = Config("localhost", 8080)
  val requests: Requests = Requests.empty

  type Effect[A] = EitherT[IO, Error, A]

  class MyApplicativeAsk[F[_] : Applicative, E](e: E) extends DefaultApplicativeAsk[F, E] {
    override val applicative: Applicative[F] = Applicative[F]

    override def ask: F[E] = e.pure
  }

  class AtomicMonadState[F[_] : Monad, S](value: AtomicReference[S]) extends MonadState[F, S] {
    override val monad: Monad[F] = Monad[F]

    override def get: F[S] = value.get().pure

    override def set(s: S): F[Unit] = value.set(s).pure

    override def inspect[A](f: S => A): F[A] = get.map(f)

    override def modify(f: S => S): F[Unit] = get.flatMap(f andThen set)
  }
  object AtomicMonadState {
    def create[F[_] : Monad, S](s: S): AtomicMonadState[F, S] = new AtomicMonadState(new AtomicReference[S](s))
  }

  implicit val configAsk: ConfigAsk[Effect] = new MyApplicativeAsk(config)
  implicit val console: Console[Effect] = new Console[Effect] {
    override def println(line: String): Effect[Unit] = EitherT.liftF(IO(scala.Console.println(line)))

    override def readLine: Effect[String] = EitherT.liftF(IO(scala.io.StdIn.readLine()))
  }
  implicit val weather: Weather[Effect] = new Weather[Effect] {
    val client = new WeatherClient(config.host, config.port)

    override def forcast(city: City): Effect[Forcast] = EitherT.liftF(IO(client.forcast(city)))
  }
  implicit val requestsState: RequestsState[Effect] = AtomicMonadState.create(requests)
  implicit val errorHandler: ErrorHandler[Effect] = new ErrorHandler[Effect] {
    override val functor: Functor[Effect] = Functor[Effect]

    override def raise[A](e: Error): Effect[A] = EitherT.leftT[IO, A](e)
  }

  WeatherApp.program[Effect]
    .valueOr(error => console.println(s"Encountered an error: $error"))
    .unsafeRunSync()
}

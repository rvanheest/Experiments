package experiments.taglessfinal.weather.scalaz_impl

import experiments.taglessfinal.weather.scalaz_impl.City.ErrorHandler
import experiments.taglessfinal.weather.scalaz_impl.Requests.Requests
import scalaz.Applicative
import scalaz.zio.interop.scalaz72._
import scalaz.zio.{ App, IO }

object Main extends App {

  val config = Config("localhost", 8080)
  val requests: Requests = Requests.empty

  type Effect[A] = IO[Error, A]

  implicit val configAsk: ApplicativeAsk[Effect, Config] = ApplicativeAsk.constant[Effect, Config](config)(ioInstances)

  implicit val console: Console[Effect] = new Console[Effect] {
    override def println(line: String): Effect[Unit] = IO.now(scala.Console.println(line))

    override def readLine: Effect[String] = IO.now(scala.io.StdIn.readLine())
  }

  implicit val weather: Weather[Effect] = new Weather[Effect] {
    val client = new WeatherClient(config.host, config.port)

    override def forcast(city: City): Effect[Forcast] = IO.sync(client.forcast(city))
  }

  implicit val monadState: AtomicMonadState[Requests] = AtomicMonadState.create(requests)

  implicit val errorHandler: ErrorHandler[Effect] = new ApplicativeError[IO[Error, ?], Error] {
    override val applicative: Applicative[IO[Error, ?]] = Applicative[IO[Error, ?]](ioInstances)

    override def raiseError[A](e: Error): IO[Error, A] = IO.fail(e)
  }

  override def run(args: List[String]): IO[Nothing, Main.ExitStatus] = {
    WeatherApp.program[Effect](configAsk, console, weather, monadState, errorHandler, ioInstances)
      .redeem(error => console.println(s"Encountered an error: $error"), _ => IO.unit)
      .forever
      .attempt
      .map(_.fold(_ => 1, _ => 0))
      .map(ExitStatus.ExitNow(_))
  }
}

package experiments.taglessfinal.weather.scalaz_impl

import scalaz.Applicative

import scala.language.higherKinds

case class Config(host: String, port: Int)

object Config {

  type ConfigAsk[F[_]] = ApplicativeAsk[F, Config]
  def ConfigAsk[F[_]](implicit F: ConfigAsk[F]): ConfigAsk[F] = F

  def host[F[_] : ConfigAsk]: F[String] = ConfigAsk[F].reader(_.host)

  def port[F[_] : ConfigAsk]: F[Int] = ConfigAsk[F].reader(_.port)
}

trait ApplicativeAsk[F[_], E] {
  val applicative: Applicative[F]

  def ask: F[E]

  def reader[A](f: E => A): F[A]
}
object ApplicativeAsk {
  def constant[F[_] : Applicative, E](e: E): ApplicativeAsk[F, E] = new ApplicativeAsk[F, E] {
    override val applicative: Applicative[F] = Applicative[F]

    override def ask: F[E] = applicative.point(e)

    override def reader[A](f: E => A): F[A] = applicative.map(ask)(f)
  }
}

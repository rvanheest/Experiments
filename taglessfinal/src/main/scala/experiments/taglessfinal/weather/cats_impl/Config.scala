package experiments.taglessfinal.weather.cats_impl

import cats.mtl.ApplicativeAsk

import scala.language.higherKinds

case class Config(host: String, port: Int)

object Config {

  type ConfigAsk[F[_]] = ApplicativeAsk[F, Config]

  def ConfigAsk[F[_]](implicit F: ConfigAsk[F]): ApplicativeAsk[F, Config] = F

  def host[F[_] : ConfigAsk]: F[String] = ConfigAsk.reader(_.host)

  def port[F[_] : ConfigAsk]: F[Int] = ConfigAsk.reader(_.port)
}

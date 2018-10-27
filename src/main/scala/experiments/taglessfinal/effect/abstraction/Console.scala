package experiments.taglessfinal.effect.abstraction

import scala.language.higherKinds

trait Console[F[_]] {
  def println(s: String): F[Unit]

  def readLine(): F[String]
}
object Console {
  def apply[F[_]](implicit F: Console[F]): Console[F] = F
}

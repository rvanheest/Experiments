package experiments.taglessfinal.effect.cats

import scala.language.higherKinds

trait Console[F[_]] {
  def println(s: ConsoleOut): F[Unit]

  def readLine(): F[String]
}
object Console {
  def apply[F[_]](implicit F: Console[F]): Console[F] = F
}

package experiments.taglessfinal.effect.abstraction

import scala.language.higherKinds

trait Random[F[_]] {
  def nextInt(upper: Int): F[Int]
}
object Random {
  def apply[F[_]](implicit F: Random[F]): Random[F] = F
}

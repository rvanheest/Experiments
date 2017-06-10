import monocle.Traversal

import scala.language.higherKinds
import scalaz.Applicative
import scalaz.std.map._
import scalaz.syntax.traverse._
import scalaz.syntax.applicative._

def filterKey[K, V](predicate: K => Boolean): Traversal[Map[K, V], V] = {
  new Traversal[Map[K, V], V] {
    def modifyF[F[_] : Applicative](f: V => F[V])(s: Map[K, V]): F[Map[K, V]] = {
      s.map { case (k, v) => k -> (if (predicate(k)) f(v) else v.pure[F]) }.sequenceU
    }
  }
}

val m = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "Four")

val filterEven = filterKey[Int, String](_ % 2 == 0)
filterEven.modify(_.toUpperCase)(m)

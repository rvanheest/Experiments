package experiments.cats.mtl

import cats._
import cats.data._
import cats.implicits._
import cats.mtl.MonadState
import cats.mtl.implicits._

import scala.language.higherKinds

object MonadStateExample extends App {

  case class ServiceResult(id: Int, companies: List[String])

  // a fake call to some external service, impure, so don't do this at home!
  def serviceCall[F[_]: Monad](id: String): F[ServiceResult] = {
    println(show"Called service with $id")
    ServiceResult(0, List("My Company")).pure[F]
  }

  type Cache = Map[String, ServiceResult]

  type Cached[F[_]] = MonadState[F, Cache]
  def Cached[F[_]](implicit F: Cached[F]): Cached[F] = F

  def serviceCallAndWriteToCache[F[_] : Cached : Monad](id: String): F[ServiceResult] = for {
    result <- serviceCall[F](id)
    cache <- Cached[F].get
    _ <- Cached[F].set(cache.updated(id, result))
  } yield result

  def cachedServiceCall[F[_] : Cached : Monad](id: String): F[ServiceResult] = for {
    cache <- Cached[F].get
    result <- cache.get(id).fold(serviceCallAndWriteToCache[F](id))(_.pure[F])
  } yield result

  def invalidate[F[_] : Cached]: F[Unit] = Cached[F].set(Map.empty)

  def program[F[_] : Cached : Monad]: F[ServiceResult] = for {
    result1 <- cachedServiceCall[F]("ab94d2")
    result2 <- cachedServiceCall[F]("ab94d2") // This should use the cached value
    _ <- invalidate[F]
    freshResult <- cachedServiceCall[F]("ab94d2") // This should access the service again
  } yield freshResult

  val initialCache: Cache = Map.empty
  val (result, cache) = program[State[Cache, ?]].run(initialCache).value
  println(result)
  println(cache)
}

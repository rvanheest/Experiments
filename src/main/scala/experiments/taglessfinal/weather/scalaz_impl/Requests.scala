package experiments.taglessfinal.weather.scalaz_impl

import java.util.concurrent.atomic.AtomicReference

import scalaz.MonadState
import scalaz.zio.IO

import scala.language.higherKinds

object Requests {

  type Requests = Map[City, Forcast]

  def empty: Requests = Map.empty

  def hottest(requests: Requests): (City, Forcast) = {
    requests.maxBy { case (_, Forcast(Temperature(value, _))) => value }
  }

  type RequestsState[F[_]] = MonadState[F, Requests]

  def hottestCity[F[_] : RequestsState]: F[(City, Temperature)] = {
    implicitly[RequestsState[F]].gets(requests => {
      val (city, Forcast(temperature)) = Requests.hottest(requests)
      (city, temperature)
    })
  }
}

class AtomicMonadState[S](value: AtomicReference[S]) extends MonadState[IO[Error, ?], S] {
  override def init: IO[Error, S] = IO.sync(value.get())

  override def get: IO[Error, S] = init

  override def put(s: S): IO[Error, Unit] = IO.sync(value.set(s))

  override def point[A](a: => A): IO[Error, A] = IO.point(a)

  override def map[A, B](fa: IO[Error, A])(f: A => B): IO[Error, B] = fa map f

  override def bind[A, B](fa: IO[Error, A])(f: A => IO[Error, B]): IO[Error, B] = fa flatMap f
}

object AtomicMonadState {
  def create[S](s: S): AtomicMonadState[S] = new AtomicMonadState[S](new AtomicReference[S](s))
}

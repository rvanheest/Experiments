package monadics.laws

import monadics.instances.Identity
import monadics.structures.{Applicative, Functor, Traverse}

import scala.language.higherKinds

trait TraverseLaws[T[_]] extends FunctorLaws[T] {

  implicit val instance: Traverse[T]

  // traverse Identity = Identity
  def traverseIdentity[A](ta: T[A]): Boolean = {
    instance.traverse(ta)(Identity(_)) == Identity(ta)
  }

  // traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
  def traverseComposition[F[_], G[_], A, B, C](ta: T[A], g: B => G[C], f: A => F[B])(implicit gApp: Applicative[G], fApp: Applicative[F]): Boolean = {
    val c1 = instance.traverse[Compose[F, G, ?], A, C](ta)((a: A) => Compose[F, G, C](fApp.map(f(a))(g)))
    val c2 = Compose[F, G, T[C]](fApp.map(instance.traverse(ta)(f))(instance.traverse(_)(g)))
    c1 == c2
  }

  // sequenceA . fmap Identity = Identity
  def sequenceIdentity[A](ta: T[A]): Boolean = {
    instance.sequence(instance.map(ta)(Identity(_))) == Identity(ta)
  }

  // sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA
  def sequenceComposition[F[_], G[_], A](ta: T[F[G[A]]])(implicit gApp: Applicative[G], fApp: Applicative[F]): Boolean = {
    val c1 = instance.sequence[Compose[F, G, ?], A](instance.map(ta)(Compose[F, G, A]))
    val c2 = Compose[F, G, T[A]](fApp.map(instance.sequence[F[?], G[A]](ta))(instance.sequence[G[?], A]))
    c1 == c2
  }

  // TODO make into separate class ???
  case class Compose[F[_], G[_], A](fga: F[G[A]])(implicit applicativeTraverse: Applicative[Compose[F, G, ?]], fEv: Functor[F], gEv: Functor[G]) {
    def map[B](f: A => B): Compose[F, G, B] = {
      Compose[F, G, B](fEv.map(fga)(gEv.map(_)(f)))
    }
  }

  object Compose {
    implicit def composeIsApplicative[F[_], G[_]](implicit fEv: Applicative[F], gEv: Applicative[G]): Applicative[Compose[F, G, ?]] = new Applicative[Compose[F, G, ?]] {
      override def create[A](a: A): Compose[F, G, A] = {
        Compose[F, G, A](fEv.create(gEv.create(a)))
      }

      override def <*>[A, B](appFunc: Compose[F, G, A => B], appA: Compose[F, G, A]): Compose[F, G, B] = {
        val Compose(fgab) = appFunc
        val Compose(fga) = appA

        val y: F[G[A] => G[B]] = fEv.map(fgab)((gAtoB: G[A => B]) => (ga: G[A]) => gEv.<*>(gAtoB, ga))
        Compose[F, G, B](fEv.<*>(y, fga))
      }
    }
  }
}

object TraverseLaws {
  def apply[T[_]](implicit t: Traverse[T]): TraverseLaws[T] = new TraverseLaws[T] {
    val instance: Traverse[T] = t
  }
}

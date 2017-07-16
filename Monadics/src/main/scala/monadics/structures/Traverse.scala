package monadics.structures

import scala.language.higherKinds

trait Traverse[T[_]] extends Functor[T] with Foldable[T] {

  def traverse[G[_], A, B](fa: T[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[T[B]]

  def sequence[G[_], A](fa: T[G[A]])(implicit applicative: Applicative[G]): G[T[A]] = traverse(fa)(ga => ga)
}

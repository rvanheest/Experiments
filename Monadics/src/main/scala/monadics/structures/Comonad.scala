package monadics.structures

import scala.language.higherKinds

trait Comonad[W[_]] extends Functor[W] {

  def extract[A](comonad: W[A]): A

  def extend[A, B](comonad: W[A])(f: W[A] => B): W[B]
}

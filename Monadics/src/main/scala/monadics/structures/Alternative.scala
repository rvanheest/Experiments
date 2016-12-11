package monadics.structures

import scala.language.higherKinds

trait Alternative[Alt[_]] extends Applicative[Alt] with MonoidK[Alt] {

	def atLeastOnce[A](alt: Alt[A]): Alt[List[A]] = {
		atLeastOnce_v(alt)
	}

	def many[A](alt: Alt[A]): Alt[List[A]] = {
		many_v(alt)
	}

	private def many_v[A](alt: Alt[A]): Alt[List[A]] = {
		combine(atLeastOnce_v(alt), create(Nil))
	}

	private def atLeastOnce_v[A](alt: Alt[A]): Alt[List[A]] = {
		<*>(map[A, List[A] => List[A]](alt)(a => a :: _), many_v(alt))
	}

	def maybe[A](alt: Alt[A]): Alt[Option[A]] = {
		combine(map(alt)(Some(_)), create(None))
	}
}

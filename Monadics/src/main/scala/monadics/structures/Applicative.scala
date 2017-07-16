package monadics.structures

import scala.language.higherKinds

trait Applicative[App[_]] extends Functor[App] {
	def create[A](a: A): App[A]

	def <*>[A, B](appFunc: App[A => B], appA: App[A]): App[B]

	override def map[A, B](functor: App[A])(f: A => B): App[B] = {
		<**>(functor, create(f))
	}

	def *>[A, B](appA: App[A], appB: App[B]): App[B] = {
		<*>(map[A, B => B](appA)(_ => identity[B]), appB)
	}

	def <*[A, B](appA: App[A], appB: App[B]): App[A] = {
		<*>(map[A, B => A](appA)(a => _ => a), appB)
	}

	def <**>[A, B](appA: App[A], appFunc: App[A => B]): App[B] = {
		<*>(appFunc, appA)
	}
}

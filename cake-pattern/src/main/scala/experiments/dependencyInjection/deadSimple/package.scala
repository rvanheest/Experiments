package experiments.dependencyInjection

import experiments.dependencyInjection.deadSimple.DependecyInjectionFramework.Reader

import scala.language.implicitConversions

package object deadSimple {

  implicit def functionAsReader[T, S](f: T => S): Reader[T, S] = Reader(f)
}

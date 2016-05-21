package experiments.dependencyInjection.deadSimple

object DependecyInjectionFramework {

	case class Reader[T, S](f: T => S) {
		def apply(t: T): S = f(t)

		def map[R](g: S => R): Reader[T, R] = {
			g compose f
		}

		def flatMap[R](g: S => Reader[T, R]): Reader[T, R] = {
			Reader(t => g(f(t))(t))
		}
	}
	object Reader {
		def apply[T, S](s: => S): Reader[T, S] = Reader(_ => s)

		implicit def reader[T, S](f: T => S): Reader[T, S] = Reader(f)
	}
}

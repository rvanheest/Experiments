package experiments.codingChallenges

/*
	Given a stream of 0's and 1's
	e.g. 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0

	When a 1 is detected, the next 3 elements need to be skipped.
	e.g. 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0
 */

object StreamFilter extends App {

	def mikesFilter[T](stream: Stream[T], skipElem: T, skipN: Int): Stream[T] = {
		sealed abstract class State
		case class Value(value: T) extends State
		case class Skip(skip: Int) extends State

		stream.scanLeft(Skip(0): State) {
			case (Value(`skipElem`), _) => Skip(skipN - 1)
			case (Value(_), x) => Value(x)
			case (Skip(0), x) => Value(x)
			case (Skip(n), _) => Skip(n - 1)
		}.drop(1).collect { case Value(v) => v }
	}

	println(mikesFilter(Stream(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), 1, 3).mkString)
	println(mikesFilter(Stream(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), 1, 3).mkString)

	// 00001000000010000
	// 00001000010

	// 100001000000010000
	// 101000010
}

object StreamFilterWithObservable extends App {

	import rx.lang.scala.Observable

	def mikesFilter[T](stream: Observable[T], skipElem: T, skipN: Int): Observable[T] = {
		stream.publish(s => (s.takeUntil(_ == skipElem) ++ s.take(skipN).drop(skipN)).repeat.takeUntil(s.lastOrElse(0)))
	}

	mikesFilter(Observable.just(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), 1, 3)
	  .foldLeft("")(_ + _)
	  .subscribe(println(_))

	mikesFilter(Observable.just(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), 1, 3)
		.foldLeft("")(_ + _)
		.subscribe(println(_))

	mikesFilter(Observable.just(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0), 1, 3)
		.foldLeft("")(_ + _)
		.subscribe(println(_))

	mikesFilter(Observable.just(0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), 1, 3)
		.foldLeft("")(_ + _)
		.subscribe(println(_))

	// 00001000000010000
	// 00001000010

	// 100001000000010000
	// 101000010

	// 0000100000001000010
	// 000010000101

	// 00001010000010000
	// 00001000010
}

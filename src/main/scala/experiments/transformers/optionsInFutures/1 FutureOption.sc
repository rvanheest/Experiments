import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

// we want to add two numbers that are both in an Option
val o1 = Option(5)
val o2 = Option(3)

// the following two methods do the same
// the latter desugers to the former
o1.flatMap(x => o2.map(y => x + y))
for {
  x <- o1
  y <- o2
} yield x + y

// this can be done with any object that implements map/flatMap
val f1 = Future(5)
val f2 = Future(3)

f1.flatMap(x => f2.map(y => x + y))
for {
  x <- f1
  y <- f2
} yield x + y

// problem when for example futures and options are nested
val fo1 = Future(Option(5))
val fo2 = Future(Option(3))

// for comprehensions don't work here
//for {
//  x <- fo1
//  y <- fo2
//} yield x + y

// we can solve this with some more maps/flatMaps
// but it is ugly, weird and gets more complex if
// you have more than two levels of nesting
fo1.flatMap(ox => fo2.map(oy =>
  ox.flatMap(x => oy.map(y => x + y))))

// pattern matching would be an option too
fo1.flatMap {
  case None => Future.successful(None)
  case Some(x) => fo2.map {
    case None => None
    case Some(y) => Some(x + y)
  }
}

// map and flatMap only work one level deep
// solution is to create a new wrapper
case class FutureOption[A](inner: Future[Option[A]]) {
  def map[B](f: A => B): FutureOption[B] = {
    FutureOption(inner.map(_ map f))
  }

  def flatMap[B](f: A => FutureOption[B]): FutureOption[B] = {
    FutureOption(inner.flatMap(_.map(f.andThen(_.inner)).getOrElse(Future.successful(None))))
  }
}

// now we can use the for-comprehension again
val foRes = for {
  x <- FutureOption(fo1)
  y <- FutureOption(fo2)
} yield x + y
foRes.inner

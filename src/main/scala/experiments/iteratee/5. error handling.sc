import scala.io.Source
import scala.util.{Failure, Success, Try}

val iterator1 = Source.fromFile(getClass.getResource("/iteratee/foo.txt").toURI).getLines()
val iterator2 = Source.fromFile(getClass.getResource("/iteratee/foo.txt").toURI).getLines()

def enumerate[In, Out](iterator: Iterator[In])(init: Out)(f: (Out, In) => Out): Try[Out] = Try {
  var result = init
  iterator.foreach(line => result = f(result, line))

  result
}

enumerate(iterator1)(0)(_ + _.length)
enumerate(iterator2)(0)(_ + _.toInt)

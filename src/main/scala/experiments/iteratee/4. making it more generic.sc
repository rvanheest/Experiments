import scala.io.Source

val iterator = Source.fromFile(getClass.getResource("/iteratee/foo.txt").toURI).getLines()

def enumerate[In, Out](iterator: Iterator[In])(init: Out)(f: (Out, In) => Out): Out = {
  var result = init
  iterator.foreach(line => result = f(result, line))

  result
}

enumerate(iterator)(0)(_ + _.length)

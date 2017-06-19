import scala.io.Source

val iterator = Source.fromFile(getClass.getResource("/iteratee/foo.txt").toURI).getLines()

def enumerate(iterator: Iterator[String])(init: Int)(f: (Int, String) => Int): Int = {
  var result = init
  iterator.foreach(line => result = f(result, line))

  result
}

enumerate(iterator)(0)(_ + _.length)

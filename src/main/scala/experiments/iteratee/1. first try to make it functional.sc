import scala.io.Source

val iterator = Source.fromFile(getClass.getResource("/iteratee/foo.txt").toURI).getLines()

var result = 0
iterator.foreach(line => result += line.length)

result

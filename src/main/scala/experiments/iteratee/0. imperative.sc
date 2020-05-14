import scala.io.Source

val iterator = Source.fromFile(getClass.getResource("/iteratee/foo.txt").toURI).getLines()

var result = 0
while (iterator.hasNext) {
  val line = iterator.next()
  result += line.length
}

result

/*
  this is a repetitive pattern (DRY principle)
  you need to do manual pulling
  there is mutability, imperative style
  there is no error handling
  we don't close the resource
  no or difficult composability
  what if the input stream were infinite?
 */

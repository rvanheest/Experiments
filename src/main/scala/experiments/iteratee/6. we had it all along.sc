import scala.io.Source
import scala.language.postfixOps
import scala.util.Try

val iterator = Source.fromFile(getClass.getResource("/iteratee/foo.txt").toURI).getLines()
iterator.foldLeft(Try(0)) {
  // f :: (Out, In) => Out
  case (acc, line) => acc.map(line.length +)
}

/*
  producer and consumer cannot talk back and forth
  still it cannot cope with infinite streams
  who is doing the resource handling (stick this into another function?)
  how about asynchronism (could have used Futures)
  data might not be available all at once
 */

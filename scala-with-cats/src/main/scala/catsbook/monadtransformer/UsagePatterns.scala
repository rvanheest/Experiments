package catsbook.monadtransformer

import cats.data.{ OptionT, Writer }
import cats.instances.list._

import scala.util.Try

object UsagePatterns extends App {

  type Logged[A] = Writer[List[String], A]

  def parseNumber(str: String): Logged[Option[Int]] = {
    Try { str.toInt }.toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None => Writer(List(s"Failed on $str"), None)
    }
  }

  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c

    result.value
  }

  println(addAll("1", "2", "3")) // WriterT((List("Read 1", "Read 2", "Read 3"), Some(6)))
  println(addAll("1", "a", "3")) // WriterT((List("Read 1", "Failed on a"), None))
}

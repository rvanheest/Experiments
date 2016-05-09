package experiments.parsecrx

import experiments.parsecrx.RxParser.failure
import rx.lang.scala.Observable

import scala.language.postfixOps

object Test extends App {

  def item: RxParser[String, Char] = {
    new RxParser(_.flatMap(_.toList match {
      case x :: xs => Observable.just((x, xs.mkString))
      case Nil => Observable.empty
    }))
  }

  val input = Observable.just("abc")

  failure.parse(input).subscribe(println(_), println(_))

  RxParser.from("a").parse(input).subscribe(println(_))

  item.parse(input).subscribe(println(_))

  item.map(c => (c + 1).toChar).parse(input).subscribe(println(_))

  (for {
    a <- item
    _ <- item
    c <- item
  } yield (a :: c :: Nil).mkString).parse(input).subscribe(println(_))

  item.satisfy('a' ==).parse(input).subscribe(println(_))
  item.satisfy('b' ==).parse(input).subscribe(println(_), println(_))

  item.orElse(failure[String, Char]).parse(input).subscribe(println(_))
  failure.orElse(item).parse(input).subscribe(println(_), println(_))
  failure[String, Char].orElse(failure[String, Char]).parse(input).subscribe(println(_), println(_))
  (for { a <- item; b <- item; c <- failure[String, Char] } yield (a :: b :: c :: Nil).mkString)
    .orElse(for {a <- item; b <- item; c <- item} yield (a :: b :: c :: Nil).mkString)
    .parse(input)
    .subscribe(println(_), println(_))


}

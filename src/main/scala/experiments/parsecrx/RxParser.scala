package experiments.parsecrx

import experiments.parsecrx.RxParser._
import rx.lang.scala.Observable

//sealed abstract class Result[A, S]
case class RxParserFailure() extends Exception

class RxParser[S, A](val parse: Observable[S] => Observable[(A, S)]) {

  def run(input: Observable[S]): Observable[A] = {
    parse(input).map(_._1)
  }

  def map[B](f: A => B): RxParser[S, B] = {
    RxParser(parse(_).map { case (a, s) => (f(a), s) })
  }

  def flatMap[B](f: A => RxParser[S, B]): RxParser[S, B] = {
    RxParser(input => for {
      (v, out) <- parse(input)
      result <- f(v).parse(Observable.just(out))
    } yield result)
  }

  def orElse[B >: A](other: => RxParser[S, B]): RxParser[S, B] = {
    RxParser(input => parse(input).onErrorResumeNext {
      case RxParserFailure() => other.parse(input)
      case e => Observable.error(e)
    })
  }

  def satisfy(predicate: A => Boolean): RxParser[S, A] = {
    flatMap(x => if (predicate(x)) from(x) else failure)
  }

  def noneOf(as: List[A]): RxParser[S, A] = {
    satisfy(!as.contains(_))
  }

  def maybe: RxParser[S, Option[A]] = {
    map(Option(_)) orElse from(Option.empty)
  }

  def many: RxParser[S, List[A]] = atLeastOnce orElse from(Nil)

  def atLeastOnce: RxParser[S, List[A]] = {
    flatMap (v => many map (v :: _))
  }
}
object RxParser {
  def apply[S, A](parser: Observable[S] => Observable[(A, S)]) = new RxParser(parser)

  def from[S, A](a: A): RxParser[S, A] = RxParser(obs => obs.map((a, _)))

  def failure[S, A]: RxParser[S, A] = RxParser(_ => Observable.error(new RxParserFailure()))
}

package experiments.cats.exercises

import cats.Semigroup
import cats.data.Validated.{Invalid, Valid}
import cats.data.Validated
import cats.data.NonEmptyList

object validation extends App {

  case class ConnectionParams(url: String, port: Int)

  trait Read[A] {
    def read(s: String): Option[A]
  }

  object Read {
    def apply[A](implicit ev: Read[A]): Read[A] = ev

    implicit val stringRead: Read[String] = {
      new Read[String] {
        def read(s: String): Option[String] = Some(s)
      }
    }

    implicit val intRead: Read[Int] = {
      new Read[Int] {
        def read(s: String): Option[Int] =
          if (s.matches("-?[0-9]+")) Some(s.toInt)
          else None
      }
    }
  }

  sealed abstract class ConfigError
  final case class MissingConfig(field: String) extends ConfigError
  final case class ParseError(field: String) extends ConfigError

  case class Config(map: Map[String, String]) {
    def parse[A: Read](key: String): Validated[ConfigError, A] = {
      map.get(key)
        .map(Read[A].read(_)
          .map(Valid(_))
          .getOrElse(Invalid(ParseError(key))))
        .getOrElse(Invalid(MissingConfig(key)))
    }
  }

  def parallelValidate[E: Semigroup, A, B, C](v1: Validated[E, A], v2: Validated[E, B])(f: (A, B) => C): Validated[E, C] =
    (v1, v2) match {
      case (Valid(a), Valid(b)) => Valid(f(a, b))
      case (Valid(_), i @ Invalid(_)) => i
      case (i @ Invalid(_), Valid(_)) => i
      case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
    }

  val config = Config(Map(("url", "127.0.0.1"), ("port", "1337")))
  val valid = parallelValidate(
    config.parse[String]("url").toValidatedNel,
    config.parse[Int]("port").toValidatedNel
  )(ConnectionParams.apply)
  println(valid.isValid)
  println(valid.getOrElse(ConnectionParams("", 0)))
  println

  val config2 = Config(Map(("endpoint", "127.0.0.1"), ("port", "not a number")))

  val invalid = parallelValidate(
    config2.parse[String]("url").toValidatedNel,
    config2.parse[Int]("port").toValidatedNel
  )(ConnectionParams.apply)
  println(invalid.isValid)
  val errors = NonEmptyList(MissingConfig("url"), List(ParseError("port")))
  println(invalid == Validated.invalid(errors))
}

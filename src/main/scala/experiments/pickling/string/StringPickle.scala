package experiments.pickling.string

import experiments.parsec.Parser
import experiments.parsec.string.StringParser
import experiments.parsec.string.StringParser.StringParser
import experiments.pickling.{ Pickle, PickleBuilder }

import scala.language.postfixOps

case class StringPickle[A](override val pickle: (A, String) => String,
                           override val unpickle: StringParser[A])
  extends Pickle[A, String](pickle, unpickle) {

  type Repr[X] = StringPickle[X]

  protected[this] def builder[X]: PickleBuilder[X, String, StringPickle[X]] = StringPickle.stringPickleBuilder
}

object StringPickle {
  implicit def stringPickleBuilder[X]: PickleBuilder[X, String, StringPickle[X]] = {
    new PickleBuilder[X, String, StringPickle[X]] {
      def apply(pickle: (X, String) => String, unpickle: Parser[String, X]): StringPickle[X] = {
        StringPickle(pickle, unpickle)
      }
    }
  }

  def item: StringPickle[Char] = {
    StringPickle(
      pickle = _ +: _,
      unpickle = StringParser.item
    )
  }

  def digit: StringPickle[Char] = item.satisfy(_.isDigit)

  def number: StringPickle[String] = digit.atLeastOnce.seq[String](_.toList).map(_.mkString)

  def lower: StringPickle[Char] = item.satisfy(_.isLower)

  def upper: StringPickle[Char] = item.satisfy(_.isUpper)

  def letter: StringPickle[Char] = item.satisfy(_.isLetter)

  def alphanum: StringPickle[Char] = item.satisfy(c => c.isLetter || c.isDigit)

  def char(c: Char): StringPickle[Char] = item.satisfy(c ==)

  def space: StringPickle[Char] = char(' ')

  def string(s: String): StringPickle[String] = s.toList match {
    case x :: xs => for {
      _ <- char(x).seq[String](_.head)
      _ <- string(xs.mkString).seq[String](_.tail)
    } yield s
    case Nil => Pickle.lift("")
  }
}

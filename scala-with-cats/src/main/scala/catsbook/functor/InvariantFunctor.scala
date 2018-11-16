package catsbook.functor

object InvariantFunctor extends App {

  trait Codec[A] {
    self =>
    def encode(value: A): String

    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): B = dec(self.decode(value))
    }
  }
  object Codec {
    def apply[A](implicit codec: Codec[A]): Codec[A] = codec
  }

  def encode[A: Codec](value: A): String = Codec[A].encode(value)

  def decode[A: Codec](value: String): A = Codec[A].decode(value)

  implicit val stringCodec: Codec[String] = new Codec[String] {
    override def encode(value: String): String = value

    override def decode(value: String): String = value
  }
  implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)
  implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)
  implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)

  case class Box[A](value: A)

  implicit def boxCodec[A: Codec]: Codec[Box[A]] = Codec[A].imap(Box(_), _.value)

  println(encode(123.4)) // "123.4"
  println(decode[Double]("123.4")) // 123.4

  println(encode(Box(123.4))) // "123.4"
  println(decode[Box[Double]]("123.4")) // Box(123.4)
}

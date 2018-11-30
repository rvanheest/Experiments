package catsbook.monad

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

object CatsWriterMonad extends App {

  type Logged[A] = Writer[Vector[String], A]

  println(123.pure[Logged]) // WriterT((Vector(), 123)

  println(Vector("msg1", "msg2", "msg3").tell) // WriterT((Vector("msg1", "msg2", "msg3"), ()))

  val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
  val b = 123.writer(Vector("msg1", "msg2", "msg3"))

  println(a.value) // 123
  println(a.written) // Vector("msg1", "msg2", "msg3")
  println(a.run) // (Vector("msg1", "msg2", "msg3"), 123)

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b
  println(writer1.run) // (Vector("a", "b", "c", "x", "y", "z"), 42)

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
  println(writer2.run) // (Vector("A", "B", "C", "X", "Y", "Z"), 42)

  val writer3 = writer1.bimap(_.map(_.toUpperCase), _ * 100)
  println(writer3.run) // (Vector("A", "B", "C", "X", "Y", "Z"), 4200)

  val writer4 = writer1.mapBoth((log, res) => {
    val log2 = log.map(_ + "!")
    val res2 = res * 1000

    (log2, res2)
  })
  println(writer4.run) // (Vector("a!", "b!", "c!", "x!", "y!", "z!"), 42000)

  val writer5 = writer1.reset
  println(writer5.run) // (Vector(), 42)

  val writer6 = writer1.swap
  println(writer6.run) // (42, Vector("a", "b", "c", "x", "y", "z"))
}

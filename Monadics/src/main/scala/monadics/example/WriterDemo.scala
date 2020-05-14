package monadics.example

import monadics.instances.Writer
import monadics.instances.list.listIsMonoid

object WriterDemo extends App {

  type Log = List[String]

  val writer = for {
    _ <- Writer.tell[Log, Unit](List("start"))
    x <- Writer(2 + 2, List("addition"))
    y <- Writer(2 * x, List("multiplication"))
    _ <- Writer.tell(List("end"))
  } yield y

  println(writer.value)
  println(writer.log.map(s => s" - $s").mkString("Report:\n", "\n", ""))
}

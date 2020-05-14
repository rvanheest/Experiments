package monadics.example

import monadics.instances.Reader

object ReaderDemo extends App {

  case class Settings(x: Int, y: Int)

  val calculation = for {
    res1 <- Reader[Settings, Int](ctx => 1 + ctx.x)
    res2 <- Reader[Settings, Int](ctx => res1 * ctx.y)
  } yield res2

  val result = calculation(Settings(3, 4))

  // res1 = 1 + x = 1 + 3 = 4
  // res2 = res1 * 4 = 4 * 4 = 16

  println(result) // should print 16
}

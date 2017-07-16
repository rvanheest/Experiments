package experiments.better_files

import better.files._
import better.files.File._

object Instantiation extends App {

  val f1 = currentWorkingDirectory
  val foo1 = File(getClass.getResource("/iteratee/foo.txt").toURI)
  val foo2 = currentWorkingDirectory/"target"/"classes"/"iteratee"/"foo.txt"
  val foo3 = currentWorkingDirectory/"src"/"main"/"resources"/"iteratee"/"foo.txt"

  println(f1)
  println(foo1)
  println(foo2)
  println(foo3)

  println(foo1 == foo2) // shouldBe true
  println(foo1 === foo2) // shouldBe true
  println(foo1 == foo3) // shouldBe false
  println(foo1 === foo3) // shouldBe true
}

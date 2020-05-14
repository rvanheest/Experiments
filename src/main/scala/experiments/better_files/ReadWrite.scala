package experiments.better_files

import better.files.File._

object ReadWrite extends App {

  val foo = currentWorkingDirectory/"src"/"main"/"resources"/"better-files"/"foo.txt"
  if (foo.exists) foo.delete(swallowIOExceptions = true)

  val file1 = foo.createIfNotExists(createParents = true)
    .write("hello world")
    .write("test123")
  println(s"${file1.name}:")
  println(file1.contentAsString)
  println

  val bar = currentWorkingDirectory/"src"/"main"/"resources"/"better-files"/"bar.txt"
  val baz = currentWorkingDirectory/"src"/"main"/"resources"/"better-files"/"baz.markdown"
  if (bar.exists) bar.delete(swallowIOExceptions = true)
  if (baz.exists) baz.delete(swallowIOExceptions = true)

  val file2 = bar.createIfNotExists(createParents = true)
    .write("hello")
    .append(" world")
    .appendLine()
    .appendLines("test123", "foobar")
    .moveTo(bar.parent/"baz")
    .renameTo("baz.txt")
    .changeExtensionTo(".markdown")
  println(s"${file2.name}:")
  println(file2.contentAsString)
}

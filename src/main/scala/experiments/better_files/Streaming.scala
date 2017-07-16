package experiments.better_files

import better.files.File.currentWorkingDirectory

object Streaming extends App {

  val file = currentWorkingDirectory/"src"/"main"/"resources"/"better-files"/"baz.markdown"
  val content = file.lineIterator.map(s => s"'$s'").mkString("{", " - ", "}")
  println(content)
}

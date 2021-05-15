package experiments.podcast.auto

import better.files.File
import cats.syntax.option._

import scala.io.StdIn
import scala.util.Try

object FileIO {

  def workingDirectory(args: Array[String]): Try[File] = {
    defaultWorkingDirectory(args) getOrElse askWorkingDirectory()
  }

  private def defaultWorkingDirectory(args: Array[String]): Option[Try[File]] = {
    val maybeDir = if (args.length > 0) File(args(0)).some
                   else none
    maybeDir map validateWorkingDirectory
  }

  private def askWorkingDirectory(): Try[File] = {
    val dirString = StdIn.readLine("In which directory is podcasts.xml located? - ").trim
    validateWorkingDirectory(File(dirString)) orElse askWorkingDirectory()
  }

  private def validateWorkingDirectory(dir: File): Try[File] = Try {
    if (dir.notExists)
      throw new IllegalArgumentException(s"download destination '$dir' does not exist")

    if (!dir.isDirectory)
      throw new IllegalArgumentException(s"download destination '$dir' is not a directory")

    dir
  }

  def podcastsXmlLocation(dir: File): Try[File] = validatePodcastsXmlLocation(dir / "podcasts.xml")

  private def validatePodcastsXmlLocation(file: File): Try[File] = Try {
    if (file.notExists)
      throw new IllegalArgumentException(s"download destination '$file' does not exist")

    if (!file.isRegularFile)
      throw new IllegalArgumentException(s"download destination '$file' is not a file")

    file
  }
}

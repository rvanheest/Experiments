package experiments.podcast.auto

import better.files.File
import cats.syntax.option._
import org.apache.commons.io.FileUtils

import java.net.URL
import scala.util.Try

case class PodcastEpisode(id: String, title: String, date: String, url: URL) {

  def matchesAny(skippers: List[SkipEpisode]): Boolean = skippers exists (_ matchesWith this.title)

  def downloadTo(saveDirectory: File, episodeNameTemplate: EpisodeNameTemplate): Try[Option[LastEpisode]] = {
    downloadTo(saveDirectory, episodeName(episodeNameTemplate))
  }

  def downloadTo(saveDirectory: File, filename: String): Try[Option[LastEpisode]] = Try {
    val file = saveDirectory / normalizeFilename(filename)
    if (file.exists) {
      println(s"$file already exists, skip downloading $url")
      none
    }
    else {
      println(s"downloading $file from $url")
      FileUtils.copyToFile(url.toInputStream, file.toJava)
      LastEpisode(id, date).some
    }
  }

  def episodeName(episodeNameTemplate: EpisodeNameTemplate): String = {
    val tt = episodeNameTemplate.titleTemplate
      .map { case (titleMatch, titleTemplate) => title.replaceAll(titleMatch, titleTemplate) }
      .getOrElse(title)

    episodeNameTemplate.nameTemplate
      .replace("[date]", date)
      .replace("[title]", tt)
  }

  private def normalizeFilename(fileName: String): String = {
    fileName
      .replace("<", "")
      .replace(">", "")
      .replace(":", " -")
      .replace("\"", "")
      .replace("/", "")
      .replace("\\", "")
      .replace("|", "")
      .replace("?", "")
      .replace("*", "")
  }
}

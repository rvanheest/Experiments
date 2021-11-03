package experiments.podcast.auto

import better.files.File
import cats.syntax.option._
import experiments.podcast.auto.PodcastEpisode.{ inlineTemplateRegex, pieceRegex }
import org.apache.commons.io.FileUtils

import java.net.URL
import scala.util.Try
import scala.util.matching.Regex.Match

case class PodcastEpisode(id: String,
                          title: String,
                          date: String,
                          url: URL,
                          episodeNumber: Option[Int],
                          subtitle: Option[String],
                         ) {

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

  def episodeName(episodeNameTemplate: EpisodeNameTemplate): String =
    episodeNameTemplate.titleTemplate
      .fold(episodeNameFromNameTemplate(episodeNameTemplate.nameTemplate)) {
        case (titleMatch, titleTemplate) =>
          episodeNameTemplate.nameTemplate
            .replace("[date]", date)
            .replace("[title]", title.replaceAll(titleMatch, titleTemplate))
            .replace("[episode]", (episodeNumber fold "")(_.toString))
            .replace("[subtitle]", subtitle getOrElse "")
            .trim
      }

  private def episodeNameFromNameTemplate(nameTemplate: String): String =
    (inlineTemplateRegex findAllMatchIn nameTemplate)
      .map(createTitleTemplate)
      .zip(pieceRegex findAllIn nameTemplate)
      .foldLeft(nameTemplate) {
        case (epTitle, (Some(titleTemplate), templateString)) => epTitle.replace(templateString, titleTemplate)
        case (epTitle, _) => epTitle
      }

  private def createTitleTemplate(templateMatch: Match): Option[String] =
    templateMatch.subgroups.count(_ != null) match {
      case 1 => getFieldByName(templateMatch group 1)
      case 3 => getFieldByName(templateMatch group 1) map (_.replaceAll(templateMatch group 2, templateMatch group 3))
      case _ => none
    }

  private def getFieldByName(key: String): Option[String] =
    key match {
      case "title" => title.some
      case "date" => date.some
      case "episode" => episodeNumber.map(_.toString)
      case "subtitle" => subtitle
      case _ => none
    }

  private def normalizeFilename(fileName: String): String =
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

object PodcastEpisode {
  private val inlineTemplateRegex = "\\[([^]|]+)(?:\\|([^|]+)\\|([^]]+))?]+".r
  private val pieceRegex = "\\[[^]|]+(?:\\|[^|]+\\|[^]]+)?]".r
}

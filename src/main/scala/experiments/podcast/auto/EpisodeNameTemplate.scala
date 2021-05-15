package experiments.podcast.auto

import scala.xml.Node
import cats.syntax.option._

case class EpisodeNameTemplate(nameTemplate: String, titleTemplate: Option[(String, String)] = Option.empty) {

  def toXml: Node = {
    titleTemplate map {
      case (titleMatch, titleTemplate) =>
        <episodeNameTemplate titleMatch={titleMatch} titleTemplate={titleTemplate}>{nameTemplate}</episodeNameTemplate>
    } getOrElse <episodeNameTemplate>{nameTemplate}</episodeNameTemplate>
  }
}
object EpisodeNameTemplate {
  def read(xml: Node): EpisodeNameTemplate = {
    val tm = xml \@ "titleMatch"
    val tt = xml \@ "titleTemplate"
    
    EpisodeNameTemplate(
      xml.text.trim,
      if (tm.nonEmpty && tt.nonEmpty) (tm.trim, tt.trim).some
      else none
    )
  }
}

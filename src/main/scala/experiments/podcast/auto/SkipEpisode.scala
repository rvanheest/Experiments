package experiments.podcast.auto

import scala.xml.Node

case class SkipEpisode(titleMatch: String) {

  def toXML: Node = <skipEpisode titleMatch={titleMatch}/>

  def matchesWith(title: String): Boolean = title matches titleMatch
}

object SkipEpisode {
  def read(xml: Node): SkipEpisode = SkipEpisode(
    xml \@ "titleMatch",
  )
}

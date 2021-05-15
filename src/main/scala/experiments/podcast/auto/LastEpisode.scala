package experiments.podcast.auto

import scala.xml.Node

case class LastEpisode(identifier: String, publicationDate: String) {
  def toXML: Node = {
    <lastDownloadedEpisode>
      <identifier>{identifier}</identifier>
      <publicationDate>{publicationDate}</publicationDate>
    </lastDownloadedEpisode>
  }
}
object LastEpisode {
  def read(xml: Node): LastEpisode = LastEpisode(
    (xml \ "identifier").text.trim,
    (xml \ "publicationDate").text.trim,
  )
}

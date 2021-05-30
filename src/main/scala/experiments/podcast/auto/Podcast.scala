package experiments.podcast.auto

import better.files.File
import cats.syntax.option._

import java.net.URL
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try
import scala.xml.{ Node, NodeSeq, XML }

case class Podcast(title: String,
                   saveLocation: File,
                   podcastUrl: URL,
                   episodeNameTemplate: EpisodeNameTemplate,
                   lastEpisode: Option[LastEpisode],
                   skipEpisodes: Option[List[SkipEpisode]],
                  ) {

  def listEpisodesToDownload(): Try[Stream[PodcastEpisode]] = {
    readPodcastFeed()
      .map(episodes => (lastEpisode fold episodes) (lastEpisode => episodes takeWhile (lastEpisode.identifier != _.id)))
      .map(episodes => (skipEpisodes fold episodes) (skippers => episodes filterNot (_ matchesAny skippers)))
  }

  def podcastDirectory(): File = {
    if (saveLocation.exists && saveLocation.isDirectory) saveLocation
    else saveLocation.createDirectory()
  }

  def readPodcastFeed(): Try[Stream[PodcastEpisode]] = {
    Try { XML.load(podcastUrl.toInputStream) } map parseRss
  }

  private def parseRss(rss: Node): Stream[PodcastEpisode] = {
    (rss \ "channel" \ "item").toStream map parseEpisode
  }

  private def parseEpisode(xml: Node): PodcastEpisode = {
    val id = (xml \ "guid").text
    val title = (xml \ "title").head.text
    val pubDate = parseDate((xml \ "pubDate").text)

    lazy val urlFromEnclosure = (xml \ "enclosure").withFilter(_ \@ "type" matches "audio/(mpeg|mp3)").map(n => new URL(n \@ "url")).headOption
    lazy val urlFromContent = (xml \ "content").withFilter(_ \@ "medium" == "audio").map(n => new URL(n \@ "url")).headOption

    val url = (urlFromEnclosure orElse urlFromContent).head

    PodcastEpisode(id, title, pubDate, url)
  }

  private def parseDate(dateString: String): String = {
    val inputFormatter = DateTimeFormatter.RFC_1123_DATE_TIME
    val outputFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    ZonedDateTime.parse(dateString, inputFormatter).toLocalDate format outputFormatter
  }

  def toXML: Node = {
    <podcast>
      <name>{title}</name>
      <saveLocation>{saveLocation}</saveLocation>
      <podcastUrl>{podcastUrl}</podcastUrl>
      {episodeNameTemplate.toXml}
      {lastEpisode map (_.toXML) getOrElse NodeSeq.Empty}
      {skipEpisodes map (ss => <skipEpisodes>{ss map (_.toXML)}</skipEpisodes>) getOrElse NodeSeq.Empty}
    </podcast>
  }
}
object Podcast {
  def read(xml: Node): Podcast = Podcast(
    (xml \ "name").text.trim,
    File((xml \ "saveLocation").text.trim),
    new URL((xml \ "podcastUrl").text.trim),
    EpisodeNameTemplate.read((xml \ "episodeNameTemplate").head),
    (xml \ "lastDownloadedEpisode").headOption map LastEpisode.read,
    readSkipEpisodes(xml \ "skipEpisodes" \ "skipEpisode"),
  )

  private def readSkipEpisodes(xml: NodeSeq): Option[List[SkipEpisode]] = {
    if (xml.isEmpty) none
    else xml.map(SkipEpisode.read).toList.some
  }
}

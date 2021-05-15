package experiments.podcast.auto

import better.files.File
import cats.instances.stream._
import cats.instances.try_._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import experiments.podcast.auto.Podcasts.printer

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }
import scala.xml.{ Node, PrettyPrinter, XML }

case class Podcasts(podcasts: List[Podcast]) {

  def processPodcasts(podcastsXmlLocation: File): Try[Unit] = {
    def newLastEpisode(podcast: Podcast, maybeLastEpisode: Option[LastEpisode]): Podcast = {
      maybeLastEpisode map (le => podcast.copy(lastEpisode = le.some)) getOrElse podcast
    }

    def savePodcastsXml(remainingPodcasts: List[Podcast])(newCompleted: List[Podcast]): Try[List[Podcast]] = {
      Podcasts(newCompleted ++ remainingPodcasts) save podcastsXmlLocation as newCompleted
    }

    def recursionStep(completed: List[Podcast], podcast: Podcast, remainingPodcasts: List[Podcast], dir: File): Try[List[Podcast]] = {
      println(s"checking podcast '${ podcast.title }' for new episodes")

      for {
        episodes <- podcast.listEpisodesToDownload()
        _ = if (episodes.isEmpty) println("no new episodes found")
        lastEpisodes <- episodes.reverse traverse (_.downloadTo(dir, podcast.episodeNameTemplate))
        streamList <- (lastEpisodes scanLeft podcast)(newLastEpisode) drop 1 map (completed :+ _) traverse savePodcastsXml(remainingPodcasts)
      } yield streamList.lastOption getOrElse completed :+ podcast
    }

    @tailrec
    def recursion(todo: List[Podcast], completed: List[Podcast] = List.empty): Try[Unit] = {
      todo match {
        case Nil => Success(())
        case podcast :: remainingPodcasts =>
          recursionStep(completed, podcast, remainingPodcasts, podcast.podcastDirectory()) match {
            case Success(completed) => recursion(remainingPodcasts, completed)
            case Failure(e) => Failure(e)
          }
      }
    }

    recursion(podcasts)
  }

  def toXML: Node = <podcasts>{ podcasts map (_.toXML) }</podcasts>

  def save(file: File): Try[Unit] = Try {
    XML.save(file.toString(), XML.loadString(printer format toXML), xmlDecl = true)
  }
}

object Podcasts {
  private val printer = new PrettyPrinter(160, 4)

  def read(xml: Node): Podcasts = Podcasts((xml \ "podcast" map Podcast.read).toList)

  def read(file: File): Try[Podcasts] = Try { XML loadFile file.toJava } map read
}

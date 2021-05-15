package experiments.podcast

import experiments.podcast.auto.{ FileIO, Podcasts }

import scala.util.{ Failure, Success }

object AutoPodcastDownloader extends App {

  val result = for {
    workingDirectory <- FileIO.workingDirectory(args)
    podcastsXmlLocation <- FileIO.podcastsXmlLocation(workingDirectory)
    podcasts <- Podcasts.read(podcastsXmlLocation)
    _ <- podcasts.processPodcasts(podcastsXmlLocation)
  } yield ()

  result match {
    case Failure(e) => e.printStackTrace()
    case Success(_) => println("completed")
  }
}

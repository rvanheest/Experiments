package experiments.podcast

import java.net.URL
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import better.files._
import org.apache.commons.csv.CSVFormat
import org.apache.commons.io.FileUtils

import scala.io.StdIn
import scala.util.{ Failure, Success, Try }
import scala.xml.{ Node, XML }

object PodcastDownloader extends App {

  val result = for {
    url <- url()
    saveLocation <- saveLocation()
    skip <- fetchSkip()
    count <- fetchCount()
    rss <- readRss(url)
    podcast <- parseRss(rss, count, skip)
    saveDirectory = (saveLocation / normalizeFilename(podcast.title)).createIfNotExists(asDirectory = true)
    _ <- csvReport(saveDirectory, podcast)
    _ <- downloadPodcast(saveDirectory, podcast)
  } yield ()

  result match {
    case Failure(e) => e.printStackTrace()
    case Success(_) => println("completed")
  }

  case class Podcast(title: String, items: Seq[Item])
  case class Item(title: String, date: String, url: URL, description: String)

  def url(): Try[URL] = {
    val urlStr = StdIn.readLine("Podcast URL: ").trim
    Try { new URL(urlStr) } recoverWith {
      case _ => Failure(new IllegalArgumentException(s"Podcast URL '$urlStr' is not valid"))
    }
  }

  def saveLocation(): Try[File] = Try {
    val default = if (args.length > 0) Some(args(0)) else None
    val defaultString = default.fold("")(d => s" (default: $d)")
    val destStr = StdIn.readLine(s"Save downloaded files at$defaultString: ").trim
    val dest = destStr match {
      case "" => File(default getOrElse "")
      case file => File(file)
    }

    if (dest.notExists)
      throw new IllegalArgumentException(s"download destination '$dest' does not exist")

    if (!dest.isDirectory)
      throw new IllegalArgumentException(s"download destination '$dest' is not a directory")

    dest
  }

  def fetchSkip(): Try[Int] = {
    val countStr = StdIn.readLine("How many entries to skip from the start (0, 1, 2, 3, ...) (default: 0): ").trim.toLowerCase
    countStr match {
      case "" => Success(0)
      case _ => Try { countStr.toInt } recoverWith { case _ => Failure(new IllegalArgumentException("not a number")) }
    }
  }

  def fetchCount(): Try[Int] = {
    val countStr = StdIn.readLine("How many entries to fetch (1, 2, 3, ..., all) (default: all): ").trim.toLowerCase
    countStr match {
      case "" | "all" => Success { Int.MaxValue }
      case _ => Try { countStr.toInt } recoverWith { case _ => Failure(new IllegalArgumentException("not a number")) }
    }
  }

  def readRss(url: URL): Try[Node] = Try { XML.load(url) }

  def parseRss(rss: Node, itemCount: Int, skipCount: Int): Try[Podcast] = Try {
    val channel = rss \ "channel"
    val title = (channel \ "title").text
    val items = (channel \ "item").toStream.map(parseItem).drop(skipCount).take(itemCount).toList

    Podcast(title, items)
  }

  private def parseItem(item: Node): Item = {
    val title = (item \ "title").head.text
    val pubDate = parseDate((item \ "pubDate").text)
    
    lazy val urlFromEnclosure = (item \ "enclosure").withFilter(_ \@ "type" matches "audio/(mpeg|mp3)").map(n => new URL(n \@ "url")).headOption
    lazy val urlFromContent = (item \ "content").withFilter(_ \@ "medium" == "audio").map(n => new URL(n \@ "url")).headOption
    
    val url = (urlFromEnclosure orElse urlFromContent).head
    val description = (item \ "description").text

    Item(title, pubDate, url, description)
  }

  private def parseDate(dateString: String): String = {
    val inputFormatter = DateTimeFormatter.RFC_1123_DATE_TIME
    val outputFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    ZonedDateTime.parse(dateString, inputFormatter).toLocalDate.format(outputFormatter)
  }

  def csvReport(saveDirectory: File, podcast: Podcast): Try[Unit] = Try {
    val csvFormat: CSVFormat = CSVFormat.RFC4180
      .withHeader("title", "date", "url", "description")
      .withDelimiter(',')
      .withRecordSeparator('\n')
    val reportFile = saveDirectory / s"${ normalizeFilename(podcast.title) }.csv"

    for (fileWriter <- reportFile.fileWriter(append = true);
         format = if (reportFile.exists) csvFormat.withSkipHeaderRecord() else csvFormat;
         printer <- format.print(fileWriter).autoClosed;
         Item(title, date, url, description) <- podcast.items) {
      printer.printRecord(title, date, url, description)
    }
  }

  def downloadPodcast(saveDirectory: File, podcast: Podcast): Try[Unit] = Try {
    for (Item(title, date, url, _) <- podcast.items) {
      val titleForFilename = normalizeFilename(title)
      val file = saveDirectory / s"$date $titleForFilename.mp3"
      if (file.exists)
        println(s"$file already exists, skip downloading $url")
      else {
        println(s"downloading $url to $file")
        FileUtils.copyURLToFile(url, file.toJava)
      }
    }
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

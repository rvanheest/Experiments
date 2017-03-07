package experiments.pickling
import scala.collection.immutable.Seq

trait example1a extends StringPickler {

	type URL = (String, String, Option[Int], String)
	type Bookmark = (String, URL)
	type Bookmarks = List[Bookmark]

	def url: PU[URL] = string.quad(string, maybe(natural), string)

	def bookmark: PU[Bookmark] = string.pair(url)

	def bookmarks: PU[Bookmarks] = list(bookmark)
}

trait example1b extends StringPickler {

	case class URL(protocol: String, host: String, port: Option[Int], file: String)

	sealed abstract class Bookmark
	case class Link(string: String, url: URL) extends Bookmark
	case class Folder(string: String, bookmarks: Bookmarks) extends Bookmark

	type Bookmarks = List[Bookmark]

	def url: PU[URL] = {
		string.quad(string, maybe(natural), string)
			.wrap({ case (pr, h, p, f) => URL(pr, h, p, f) })({ case URL(pr, h, p, f) => (pr, h, p, f) })
	}

	def bookmark: PU[Bookmark] = {
		def tag(bookmark: Bookmark): Int = {
			bookmark match {
				case Link(_, _) => 0
				case Folder(_, _) => 1
			}
		}

		alt(List(
			string.pair(url).wrap[Bookmark]({ case (s, u) => Link(s, u) })({ case Link(s, u) => (s, u) }),
			string.pair(bookmarks).wrap[Bookmark]({ case (s, bs) => Folder(s, bs) })({ case Folder(s, bs) => (s, bs) })
		))(tag)
	}

	def bookmarks: PU[Bookmarks] = list(bookmark)
}

object example1bMain extends example1b {

	def main(args: Array[String]): Unit = {
		val url1 = URL("http", "research.microsoft.com", Option.empty, "users/akenn")
		val l1 = Link("Andrew", url1)
//		val bms = List(l1)

		val pickled: String = url.pickle(url1)
		val unpickled: URL = url.unpickle(pickled)
//		val pickled: String = bookmark.pickle(l1)
//		val pickled: String = bookmark.pickle(bms)
//		val unpickled: Seq[Bookmark] = bookmarks.unpickle(pickled)

		println(pickled)
		println()
		println(unpickled)
	}
}

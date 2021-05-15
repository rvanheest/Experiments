package experiments.podcast

import java.io.InputStream
import java.net.URL

package object auto {

  implicit class URLExtension(val url: URL) {
    def toInputStream: InputStream = {
      val uc = url.openConnection()
      uc.addRequestProperty("User-Agent", "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)")
      uc.getInputStream
    }
  }
}

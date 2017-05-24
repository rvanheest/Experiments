package com.github.rvanheest.starbux.client.logic

import java.net.URL

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.xml.{ Elem, Utility }
import scalaj.http.{ Http, HttpResponse }

trait HttpConnectionComponent {
  this: DebugEnhancedLogging =>

  val httpConnection: HttpConnection

  trait HttpConnection {
    val baseUrl: URL

    def order(xml: Elem): HttpResponse[String] = {
      val url = new URL(baseUrl, "order")
      debug(url.toString)
      Http(url.toString).postData(Utility.trim(xml).toString()).asString
    }
  }
}

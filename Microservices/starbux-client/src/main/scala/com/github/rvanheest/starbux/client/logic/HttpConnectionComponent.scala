package com.github.rvanheest.starbux.client.logic

import java.net.URL

import scala.xml.{ Elem, Utility }
import scalaj.http.{ Http, HttpResponse }

trait HttpConnectionComponent {

  val httpConnection: HttpConnection

  trait HttpConnection {
    val baseUrl: URL

    def order(xml: Elem): HttpResponse[String] = {
      val url = new URL(baseUrl, "order")
      Http(url.toString).postData(Utility.trim(xml).toString()).asString
    }
  }
}

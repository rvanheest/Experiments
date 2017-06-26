package com.github.rvanheest.starbux.service

import java.net.{ HttpURLConnection, URL, URLEncoder }
import java.nio.charset.StandardCharsets

import org.apache.commons.io.IOUtils
import resource.managed

import scala.xml.{ Node, Utility, XML }

trait ServerTestSupportFixture {

  def callService(path: String = "service/order", method: String = "GET")(f: HttpURLConnection => Unit = _ => {}): (Int, String) = {
    new URL(s"http://localhost:20000/$path").openConnection() match {
      case conn: HttpURLConnection =>
        managed {
          conn.setConnectTimeout(1000)
          conn.setReadTimeout(1000)
          conn.setRequestMethod(method)
          f(conn)
          conn
        }
          .map(_.getResponseCode)
          .acquireAndGet {
            case code if code >= 200 && code < 300 => (code, IOUtils.toString(conn.getInputStream, StandardCharsets.UTF_8))
            case code => (code, IOUtils.toString(conn.getErrorStream, StandardCharsets.UTF_8))
          }
      case _ => throw new Exception
    }
  }

  private def encode(xml: Node) = {
    URLEncoder.encode(Utility.trim(xml).toString(), StandardCharsets.UTF_8.toString)
  }

  def order(xml: Node): (Int, Node) = {
    val (code, body) = callService(s"service/order?order=${encode(xml)}", "POST") { conn =>
      conn.addRequestProperty("Content-Type", "application/xml")
    }
    (code, XML.loadString(body))
  }

  def failing(xml: Node): (Int, String) = {
    callService(s"service/order?order=${encode(xml)}", "POST") { conn =>
      conn.addRequestProperty("Content-Type", "application/xml")
    }
  }

  def successful: (Int, String) = (200, "Starbux Coffee Service running")
}

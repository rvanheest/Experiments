package com.github.rvanheest.starbux.service

import java.net.{ HttpURLConnection, URL }
import java.nio.charset.StandardCharsets

import org.apache.commons.io.IOUtils
import resource.managed

import scala.xml.{ Node, Utility, XML }

trait ServerTestSupportFixture {

  def callService(path: String = "service/order", method: String = "GET")(f: HttpURLConnection => Unit = _ => {}): String = {
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
            case code if code >= 200 && code < 300 => IOUtils.toString(conn.getInputStream, StandardCharsets.UTF_8)
            case _ => IOUtils.toString(conn.getErrorStream, StandardCharsets.UTF_8)
          }
      case _ => throw new Exception
    }
  }

  def order(xml: Node): Node = {
    XML.loadString {
      callService(s"service/order?order=${Utility.trim(xml).toString}", "POST") { conn =>
        conn.addRequestProperty("Content-Type", "application/xml")
      }
    }
  }

  def successful: String = "Starbux Coffee Service running"
}

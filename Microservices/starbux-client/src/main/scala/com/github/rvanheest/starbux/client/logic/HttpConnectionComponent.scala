/**
 * Copyright (C) 2017 Richard van Heest
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.github.rvanheest.starbux.client.logic

import java.net.URI

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.xml.{ Elem, Utility }
import scalaj.http.{ Http, HttpResponse }

trait HttpConnectionComponent {
  this: DebugEnhancedLogging =>

  val httpConnection: HttpConnection

  trait HttpConnection {
    val baseUri: URI

    lazy val orderUri: URI = baseUri.resolve("service").resolve("order")

    def order(xml: Elem): HttpResponse[String] = {
      debug(orderUri.toString)
      Http(orderUri.toString)
        .postForm(Map("order" -> Utility.trim(xml).toString()).toSeq)
        .header("Content-Type", "application/xml")
        .asString
    }
  }
}

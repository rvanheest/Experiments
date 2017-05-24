/**
 * Copyright (C) 2017 Richard van Heest (richard.v.heest@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.github.rvanheest.starbux.service

import java.net.InetAddress

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.{ Created, Ok, ScalatraServlet }

import scala.xml.{ Utility, XML }

trait OrderServletComponent {
  this: DebugEnhancedLogging =>

  val orderServlet: StarBuxServlet

  trait StarBuxServlet extends ScalatraServlet {

    val serverPort: Int

    get("/") {
      contentType = "text/plain"
      Ok("Starbux Coffee Service running ...")
    }

    post("/") {
      val order = XML.loadString(params("order"))

      val drink = (order \ "drink").text
      val cost = drink.length.toDouble
      val orderNumber = "1234" // this could come from a database returning the ID after the order has been stored

      val result = <order>
        <drink>{drink}</drink>
        <cost>{cost}</cost>
        <next rel={s"http://localhost:$serverPort/payment"}
              uri={s"http://localhost:$serverPort/payment/order/$orderNumber"}
              type="application/xml"/>
      </order>

      Created(Utility.trim(result).toString(), headers = Map("content-type" -> "application/xml"), reason = "")
    }
  }
}

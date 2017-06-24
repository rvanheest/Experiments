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
package com.github.rvanheest.starbux.service

import com.github.rvanheest.starbux.DatabaseAccessComponent
import com.github.rvanheest.starbux.order.{ DatabaseComponent, Drink, Order, Ordered }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.{ Created, Ok, ScalatraServlet }

import scala.xml.XML

trait OrderServletComponent {
  this: DatabaseAccessComponent with DatabaseComponent with DebugEnhancedLogging =>

  val orderServlet: StarBuxServlet

  trait StarBuxServlet extends ScalatraServlet {

    val serverPort: Int

    get("/") {
      contentType = "text/plain"
      Ok("Starbux Coffee Service running ...")
    }

    post("/") {
      <order>
        <drink>
          <name>coffee</name>
          <addition>sugar</addition>
          <addition>milk</addition>
        </drink>
        <drink>
          <name>tea</name>
          <addition>honey</addition>
        </drink>
      </order>

      val xml = XML.loadString(params("order"))

      val drinks = (xml \ "drink").map(n => {
        val name = (n \ "name").text
        val additions = (n \ "addition").map(_.text).toList
        Drink(drink = name, addition = additions)
      }).toList
      val order = Order(Ordered, drinks)

      for {
        orderNumber <- databaseAccess.doTransaction(implicit connection => database.addOrder(order))
        cost <- databaseAccess.doTransaction(implicit connection => database.calculateCost(orderNumber))
      } yield {
        val response = <order>
          <drink>
            <name>coffee</name>
            <addition>sugar</addition>
            <addition>milk</addition>
          </drink>
          <drink>
            <name>tea</name>
            <addition>honey</addition>
          </drink>
          <cost>{cost}</cost>
          <next rel={s"http://localhost:$serverPort/payment"}
                uri={s"http://localhost:$serverPort/payment/order/$orderNumber"}
                type="application/xml"/>
        </order>
        contentType = "application/xml"

        Created(body = response,
          headers = Map("Location" -> s"http://localhost:$serverPort/order/$orderNumber"))
      }
    }
  }
}

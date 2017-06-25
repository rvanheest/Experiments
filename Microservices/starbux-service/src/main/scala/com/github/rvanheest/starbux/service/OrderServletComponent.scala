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

import java.net.URL

import com.github.rvanheest.starbux.DatabaseAccessComponent
import com.github.rvanheest.starbux.order.{ DatabaseComponent, Drink, Order, Ordered }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.error._
import org.scalatra.{ Created, InternalServerError, Ok, ScalatraServlet }

import scala.util.Try
import scala.xml.{ Node, XML }

trait OrderServletComponent {
  this: DatabaseAccessComponent with DatabaseComponent with DebugEnhancedLogging =>

  val orderServlet: OrderServlet

  trait OrderServlet extends ScalatraServlet {

    val baseUrl: URL

    logger.info("Starbux Coffee Order Servlet running ...")

    get("/") {
      contentType = "text/plain"
      Ok("Starbux Coffee Service running")
    }

    def parseInput(input: Node): Try[Order] = Try {
      val drinks = (input \ "drink").map(n => {
        val name = (n \ "name").text
        val additions = (n \ "addition").map(_.text).toList
        Drink(drink = name, addition = additions)
      }).toList
      Order(Ordered, drinks)
    }

    post("/") {
      contentType = "application/xml"

      val result = for {
        input <- Try { XML.loadString(params("order")) }
        order <- parseInput(input)
        orderNumber <- databaseAccess.doTransaction(implicit connection => database.addOrder(order))
        cost <- databaseAccess.doTransaction(implicit connection => database.calculateCost(orderNumber))
      } yield {
        val append = Seq(
          <cost>{cost}</cost>,
          <next rel={baseUrl.toURI.resolve("payment").toString}
                uri={baseUrl.toURI.resolve(s"/payment/order/$orderNumber").toString}
                type="application/xml"/>
        )
        Created(headers = Map("Location" -> baseUrl.toURI.resolve(s"/order/$orderNumber").toString),
          body = input.copy(child = input.child ++ append))
      }

      result.getOrRecover(e => {
        logger.error(e.getMessage, e)
        InternalServerError(e.getMessage)
      })
    }
  }
}

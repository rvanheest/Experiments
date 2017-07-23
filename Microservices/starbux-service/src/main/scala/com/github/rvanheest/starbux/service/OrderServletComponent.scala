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
import com.github.rvanheest.starbux.order._
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra._

import scala.util.{ Failure, Success, Try }
import scala.xml.{ Node, XML }

trait OrderServletComponent {
  this: OrderManagementComponent with DatabaseAccessComponent with DebugEnhancedLogging =>

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
        Drink(drink = name, additions = additions)
      }).toList
      Order(Ordered, drinks)
    }

    post("/") {
      contentType = "application/xml"

      val result = for {
        input <- Try { XML.loadString(params("order")) }
        order <- parseInput(input)
        _ <- if (order.drinks.isEmpty) Failure(EmptyRequestException())
             else Success(())
        (orderId, cost) <- databaseAccess.doTransaction(implicit connection =>
          for {
            orderNumber <- orderManagement.addOrder(order)
            cost <- orderManagement.calculateCost(orderNumber)
          } yield (orderNumber, cost)
        )
      } yield {
        val append = Seq(
          <cost>{cost}</cost>,
          <next rel={baseUrl.toURI.resolve("payment").toString}
                uri={baseUrl.toURI.resolve(s"/payment/order/$orderId").toString}
                type="application/xml"/>
        )
        Created(headers = Map("Location" -> baseUrl.toURI.resolve(s"/order/$orderId").toString),
          body = input.copy(child = input.child ++ append))
      }

      result.doIfFailure { case e => logger.error(e.getMessage, e) }
        .getOrRecover {
          case e @ EmptyRequestException() => BadRequest(e.getMessage)
          case UnknownItemException(msg) => BadRequest(msg)
          case CompositeException(es) if es.forall(_.isInstanceOf[UnknownItemException]) => BadRequest(es.map(_.getMessage).mkString("\n"))
          case e => InternalServerError(e.getMessage)
        }
    }
  }
}
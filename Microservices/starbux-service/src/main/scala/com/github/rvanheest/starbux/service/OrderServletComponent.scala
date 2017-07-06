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
  this: DatabaseAccessComponent with DatabaseComponent with DebugEnhancedLogging =>

  val orderServlet: OrderServlet

  trait OrderServlet extends ScalatraServlet {

    val baseUrl: URL

    logger.info("Starbux Coffee Order Servlet running ...")

    get("/") {
      contentType = "text/plain"
      Ok("Starbux Coffee Service running")
    }

    def toXml(orderId: OrderId, order: Order, cost: Cost): Node = {
      <order>{
        order.drinks.map(drink => {
          <drink>
            <name>{drink.drink}</name>
            {drink.additions.map(addition => <addition>{addition}</addition>)}
          </drink>
        })
        <cost>{cost}</cost>
        <next rel={baseUrl.toURI.resolve("payment").toString}
              uri={baseUrl.toURI.resolve(s"/payment/order/$orderId").toString}
              type="application/xml"/>
      }</order>
    }

    get("/:orderId") {
      val response = for {
        orderId <- Try { params("orderId").toInt }
        order <- databaseAccess.doTransaction(implicit connection => database.getOrder(orderId))
        cost <- databaseAccess.doTransaction(implicit connection => database.calculateCost(orderId))
      } yield {
        Ok(headers = Map("Location" -> baseUrl.toURI.resolve(s"/order/$orderId").toString),
          body = toXml(orderId, order, cost))
      }

      response.doIfFailure { case e => logger.error(e.getMessage, e) }
        .getOrRecover {
          case e: NoSuchElementException => BadRequest(e.getMessage)
          case UnknownOrderException(msg) => BadRequest(msg)
          case UnknownOrderStateException(msg) => InternalServerError(msg + " Consult your barista for more info.")
          case e => InternalServerError(e.getMessage)
        }
    }

    def parseInput(input: Node): Try[Order] = Try {
      val drinks = (input \ "drink").map(n => {
        val name = (n \ "name").text
        val additions = (n \ "addition").map(_.text).toList
        Drink(drink = name, additions = additions)
      }).toSet
      Order(Ordered, drinks)
    }

    post("/") {
      contentType = "application/xml"

      val response = for {
        input <- Try { XML.loadString(params("order")) }
        order <- parseInput(input)
        _ <- if (order.drinks.isEmpty) Failure(EmptyRequestException())
             else Success(())
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

      response.doIfFailure { case e => logger.error(e.getMessage, e) }
        .getOrRecover {
          case e @ EmptyRequestException() => BadRequest(e.getMessage)
          case UnknownItemException(msg) => BadRequest(msg)
          case CompositeException(es) if es.forall(_.isInstanceOf[UnknownItemException]) => BadRequest(es.map(_.getMessage).mkString("\n"))
          case e => InternalServerError(e.getMessage)
        }
    }
  }
}

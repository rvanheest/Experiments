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

import java.net.URI

import com.github.rvanheest.starbux.DatabaseAccessComponent
import com.github.rvanheest.starbux.order._
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra._

import scala.util.{ Failure, Success, Try }
import scala.xml.{ Node, NodeSeq, XML }

trait OrderServletComponent {
  this: OrderManagementComponent with DatabaseComponent with DatabaseAccessComponent with DebugEnhancedLogging =>

  val orderServlet: OrderServlet

  trait OrderServlet extends ScalatraServlet {

    val baseUri: URI

    logger.info("Starbux Coffee Order Servlet running ...")

    def toXml(orderId: OrderId, order: Order, cost: Cost): Node = {
      <order>{
        order.drinks.map(drink => {
          <drink>
            <name>{drink.drink}</name>
            {drink.additions.map(addition => <addition>{addition}</addition>)}
          </drink>
        }) ++ responseXml(orderId, cost)
      }</order>
    }

    private def responseXml(orderId: OrderId, cost: Cost): NodeSeq = {
      Seq(
        <cost>{cost}</cost>,
        <next rel={baseUri.resolve("payment").toString}
              uri={baseUri.resolve(s"payment/order/$orderId").toString}
              type="application/xml"/>
      )
    }

    def parseInput(input: Node): Try[Order] = Try {
      val drinks = (input \ "drink").map(n => {
        val name = (n \ "name").text
        val additions = (n \ "addition").map(_.text).toList
        Drink(drink = name, additions = additions)
      }).toSet
      Order(Ordered, drinks)
    }

    get("/") {
      contentType = "text/plain"
      Ok("Starbux Coffee Service running")
    }

    get("/:orderId") {
      val response = for {
        orderId <- Try { params("orderId").toInt }
        (order, cost) <- databaseAccess.doTransaction(implicit connection =>
          for {
            order <- orderManagement.getOrder(orderId)
            cost <- orderManagement.calculateCost(orderId)
          } yield (order, cost)
        )
      } yield {
        Ok(headers = Map("Location" -> baseUri.resolve(s"/order/$orderId").toString),
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

    post("/") {
      contentType = "application/xml"

      val response = for {
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
      } yield Created(
        headers = Map("Location" -> baseUri.resolve(s"/order/$orderId").toString),
        body = input.copy(child = input.child ++ responseXml(orderId, cost)))

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

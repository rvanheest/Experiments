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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, XML }

trait OrdererComponent {
  this: HttpConnectionComponent
    with DebugEnhancedLogging =>

  val orderer: Orderer

  trait Orderer {
    def order(drink: String): Try[Elem] = Try {
      trace(drink)

      val response = httpConnection.order {
        <order>
          <drink>{drink}</drink>
        </order>
      }

      response.code match {
        case 201 =>
          logger.info(response.headers.map { case (k, v) => s"$k -> $v" }.mkString("[", ", ", "]"))
          Success(XML.loadString(response.body))
        case x => Failure(new Exception(
          s"""Received invalid response while placing an order.
             |  Code:    $x
             |  Headers: ${ response.headers.map { case (k, v) => s"$k -> $v" }.mkString("[", ", ", "]") }
             |  Body:    ${ response.body }""".stripMargin))
      }
    }.flatten
  }
}

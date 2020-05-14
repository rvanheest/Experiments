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
package com.github.rvanheest.starbux.order

import java.sql.Connection

import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }

trait OrderManagementComponent {
  this: DatabaseComponent with DebugEnhancedLogging =>

  val orderManagement: OrderManagement

  trait OrderManagement {
    def addOrder(order: Order)(implicit connection: Connection): Try[OrderId] = {
      for {
        orderId <- database.addToOrderTable(order.status)
        _ <- order.drinks.map(drink => for {
          _ <- database.addToOrderDrinkTable(drink, orderId)
          _ <- drink.additions.map(database.addToOrderDrinkAdditionTable(_, drink.id)).collectResults
        } yield ()).collectResults.recoverWith {
          case CompositeException(e :: Nil) => Failure(e)
          case e => Failure(e)
        }
      } yield orderId
    }

    def calculateCost(orderId: OrderId)(implicit connection: Connection): Try[Cost] = {
      database.costView(orderId)
        .flatMap {
          case Seq() => Failure(UnknownOrderException(orderId))
          case data => Try {
            data.groupBy { case (drinkId, _, _) => drinkId }
              .flatMap { case (_, values) =>
                val drinkCost = values.headOption.map { case (_, cost, _) => cost }
                val additionCost = values.map { case (_, _, cost) => cost }
                drinkCost.toSeq ++ additionCost
              }
              .sum
          }
        }
    }

    def getOrder(orderId: OrderId)(implicit connection: Connection): Try[Order] = {
      database.getOrder(orderId)
        .flatMap {
          case Seq() => Failure(UnknownOrderException(orderId))
          case Seq((status, None, _, _)) =>
            Status.fromString(status)
              .map(st => Success(Order(st, Set.empty)))
              .getOrElse(Failure(UnknownOrderStateException(orderId)))
          case data =>
            val statusAndDrinks = data
              .collect { case (status, Some(drinkId), drink, addition) => (status, drinkId, drink, addition) }
              .groupBy { case (_, drinkId, _, _) => drinkId }
              .toList
              .flatMap {
                case (_, Seq()) => None
                case (drinkId, values@Seq((status, _, drink, _), _@_*)) =>
                  val additions = values.flatMap { case (_, _, _, add) => add }.toList
                  Some(status, Drink(drinkId, drink, additions))
              }

            statusAndDrinks match {
              case Nil => Failure(UnknownOrderException(orderId))
              case order @ ((st, _) :: _) =>
                val drinks = order.map { case (_, drink) => drink }.toSet
                Status.fromString(st)
                  .map(s => Success(Order(s, drinks)))
                  .getOrElse(Failure(UnknownOrderStateException(orderId)))
            }
        }
    }

  }
}

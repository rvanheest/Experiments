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

import java.sql.{ Connection, ResultSet }
import java.util.UUID

import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import resource.managed

import scala.collection.immutable.Stream.Empty
import scala.language.postfixOps
import scala.util.{ Failure, Success, Try }

trait DatabaseComponent {
  this: DebugEnhancedLogging =>

  val database: Database

  trait Database {

    def addOrder(order: Order)(implicit connection: Connection): Try[OrderId] = {
      for {
        orderId <- addToOrderTable(order)
        _ <- order.drinks.map(drink => for {
          _ <- addToOrderDrinkTable(drink, orderId)
          _ <- drink.additions.map(addToOrderDrinkAdditionTable(_, drink.id)).collectResults
        } yield ()).collectResults.recoverWith {
          case CompositeException(e :: Nil) => Failure(e)
          case e => Failure(e)
        }
      } yield orderId
    }

    private def addToOrderTable(order: Order)(implicit connection: Connection): Try[OrderId] = {
      managed(connection.prepareStatement("INSERT INTO `Order` (statusId) SELECT Status.statusId FROM Status WHERE Status.status = ?;"))
        .map(prepStatement => {
          prepStatement.setString(1, order.status.toString)
          prepStatement.executeUpdate()
        })
        .tried
        .flatMap {
          case 1 => managed(connection.prepareStatement("SELECT last_insert_rowid();"))
            .flatMap(query => managed(query.executeQuery()))
            .map(result => {
              result.next()
              result.getInt(1)
            })
            .tried
          case _ => Failure(UnknownItemException(s"Unknown status: ${ order.status }"))
        }
    }

    private def addToOrderDrinkTable(drink: Drink, orderId: OrderId)(implicit connection: Connection): Try[Unit] = {
      managed(connection.prepareStatement("INSERT INTO order_drink (id, orderId, drinkId, drinkCost) SELECT ?, ?, Drink.drinkId, Drink.cost FROM Drink WHERE Drink.drink = ?;"))
        .map(prepStatement => {
          prepStatement.setString(1, drink.id.toString)
          prepStatement.setInt(2, orderId)
          prepStatement.setString(3, drink.drink)
          prepStatement.executeUpdate()
        })
        .tried
        .flatMap {
          case 1 => Success(())
          case _ => Failure(UnknownItemException(s"Unknown drink: ${ drink.drink }"))
        }
    }

    private def addToOrderDrinkAdditionTable(addition: Addition, drinkId: ID)(implicit connection: Connection): Try[Unit] = {
      managed(connection.prepareStatement("INSERT INTO order_drink_additions (orderDrinkId, additionId, additionCost) SELECT ?, Addition.additionId, Addition.cost FROM Addition WHERE Addition.addition = ?;"))
        .map(prepStatement => {
          prepStatement.setString(1, drinkId.toString)
          prepStatement.setString(2, addition)
          prepStatement.executeUpdate()
        })
        .tried
        .flatMap {
          case 1 => Success(())
          case _ => Failure(UnknownItemException(s"Unknown addition: $addition"))
        }
    }

    def calculateCost(orderId: OrderId)(implicit connection: Connection): Try[Int] = {
      val resultSet = for {
        prepStatement <- managed(connection.prepareStatement("SELECT status, drinkId, drinkCost, additionCost FROM OrderView WHERE orderId = ?;"))
        _ = prepStatement.setInt(1, orderId)
        resultSet <- managed(prepStatement.executeQuery())
      } yield resultSet

      def getResults(resultSet: ResultSet) = {
        val data = Stream.continually(resultSet.next())
          .takeWhile(true ==)
          .map(_ => {
            val status = resultSet.getString("status")
            val drinkId = Option(resultSet.getString("drinkId"))
            val drinkCost = resultSet.getInt("drinkCost")
            val additionCost = Option(resultSet.getInt("additionCost"))
            (status, drinkId, drinkCost, additionCost)
          })

        Option(data)
          .filterNot(_.isEmpty)
          .map(data => Success(cost(data)))
          .getOrElse(Failure(UnknownOrderException(orderId)))
      }

      def cost(data: Stream[(Addition, Option[Addition], Cost, Option[Cost])]): Cost = {
        data
          .flatMap {
            case (_, None, _, _) => Option.empty[(ID, Cost, Option[Cost])]
            case (_, Some(drinkId), drinkCost, additionCost) => Option(UUID.fromString(drinkId), drinkCost, additionCost)
          }
          .groupBy { case (drinkId, _, _) => drinkId }
          .flatMap { case (_, values) =>
            val drinkCost = values.headOption.map { case (_, cost, _) => cost }
            val additionCost = values.flatMap { case (_, _, cost) => cost }
            drinkCost.toStream #::: additionCost
          }
          .sum
      }

      resultSet.map(getResults).tried.flatten
    }

    def getOrder(orderId: OrderId)(implicit connection: Connection): Try[Order] = {
      val resultSet = for {
        prepStatement <- managed(connection.prepareStatement("SELECT status, drinkId, drink, addition FROM OrderView WHERE orderId = ?;"))
        _ = prepStatement.setInt(1, orderId)
        resultSet <- managed(prepStatement.executeQuery())
      } yield resultSet

      def getResults(resultSet: ResultSet): Try[Order] = {
        val data = Stream.continually(resultSet.next())
          .takeWhile(true ==)
          .map(_ => {
            val status = resultSet.getString("status")
            val drinkId = Option(resultSet.getString("drinkId"))
            val drink = resultSet.getString("drink")
            val addition = Option(resultSet.getString("addition"))
            (status, drinkId, drink, addition)
          })

        data match {
          case Empty => Failure(UnknownOrderException(orderId))
          case (status, None, _, _) #:: Empty =>
            Status.fromString(status)
              .map(st => Success(Order(st, Set.empty)))
              .getOrElse(Failure(UnknownOrderStateException(orderId)))
          case _ =>
            val statusAndDrinks = data
              .flatMap {
                case (_, None, _, _) => Stream.empty[(String, ID, String, Option[String])]
                case (status, Some(drinkId), drinkCost, additionCost) => (status, UUID.fromString(drinkId), drinkCost, additionCost) #:: Empty
              }
              .groupBy { case (_, drinkId, _, _) => drinkId }
              .toList
              .flatMap { case (drinkId, values) => mkDrink(drinkId, values) }

            mkOrder(statusAndDrinks)
        }
      }

      def mkDrink(drinkId: ID, values: Stream[(String, ID, String, Option[Addition])]): Option[(String, Drink)] = {
        values match {
          case Empty => None
          case (status, _, drink, _) #:: _ =>
            val additions = values.flatMap { case (_, _, _, add) => add }.toList
            Some(status, Drink(drinkId, drink, additions))
        }
      }

      def mkOrder(statusAndDrinks: List[(String, Drink)]): Try[Order] = {
        statusAndDrinks match {
          case Nil => Failure(UnknownOrderException(orderId))
          case order @ ((st, _) :: _) =>
            val drinks = order.map { case (_, drink) => drink }.toSet
            Status.fromString(st)
              .map(s => Success(Order(s, drinks)))
              .getOrElse(Failure(UnknownOrderStateException(orderId)))
        }
      }

      resultSet.map(getResults).tried.flatten
    }
  }
}

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
import java.util.UUID

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import resource.managed

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

trait DatabaseComponent {
  this: DebugEnhancedLogging =>

  type OrderId = Int
  type Cost = Int

  val database: Database

  trait Database {

    def addToOrderTable(status: Status)(implicit connection: Connection): Try[OrderId] = {
      managed(connection.prepareStatement("INSERT INTO `Order` (statusId) SELECT Status.statusId FROM Status WHERE Status.status = ?;"))
        .map(prepStatement => {
          prepStatement.setString(1, status.toString)
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
          case _ => Failure(UnknownItemException(s"Unknown status: $status"))
        }
    }

    def addToOrderDrinkTable(drink: Drink, orderId: OrderId)(implicit connection: Connection): Try[Unit] = {
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

    def addToOrderDrinkAdditionTable(addition: Addition, drinkId: ID)(implicit connection: Connection): Try[Unit] = {
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

    def costView(orderId: OrderId)(implicit connection: Connection): Try[Seq[(UUID, Cost, Cost)]] = {
      val resultSet = for {
        prepStatement <- managed(connection.prepareStatement("SELECT drinkId, drinkCost, additionCost FROM OrderView WHERE orderId = ?;"))
        _ = prepStatement.setInt(1, orderId)
        resultSet <- managed(prepStatement.executeQuery())
      } yield resultSet

      resultSet
        .map(result => {
          Stream.continually(result.next())
            .takeWhile(true ==)
            .flatMap(_ => {
              for {
                drinkId <- Option(result.getString("drinkId"))
                drinkCost = result.getInt("drinkCost")
                additionCost = result.getInt("additionCost")
              } yield (UUID.fromString(drinkId), drinkCost, additionCost)
            })
            .toList
        })
        .tried
    }
  }
}

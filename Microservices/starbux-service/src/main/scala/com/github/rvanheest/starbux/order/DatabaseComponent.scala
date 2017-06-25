package com.github.rvanheest.starbux.order

import java.sql.{ Connection, ResultSet }
import java.util.UUID

import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import resource.managed

import scala.language.postfixOps
import scala.util.Try

trait DatabaseComponent {
  this: DebugEnhancedLogging =>

  type OrderId = Int
  type Cost = Int

  val database: Database

  trait Database {

    def addOrder(order: Order)(implicit connection: Connection): Try[OrderId] = {
      for {
        orderId <- addToOrderTable(order)
        _ <- order.drinks.map(drink => for {
          _ <- addToOrderDrinkTable(drink, orderId)
          _ <- drink.addition.map(addToOrderDrinkAdditionTable(_, drink.id)).collectResults
        } yield ()).collectResults
      } yield orderId
    }

    private def addToOrderTable(order: Order)(implicit connection: Connection): Try[OrderId] = {
      managed(connection.prepareStatement("INSERT INTO `Order` (statusId) SELECT Status.statusId FROM Status WHERE Status.status = ?;"))
        .map(prepStatement => {
          prepStatement.setString(1, order.status.toString)
          prepStatement.executeUpdate()
        })
        .tried
        .flatMap(_ => managed(connection.prepareStatement("SELECT last_insert_rowid();"))
          .flatMap(query => managed(query.executeQuery()))
          .map(result => {
            result.next()
            result.getInt(1)
          })
          .tried)
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
        .map(_ => ())
    }

    private def addToOrderDrinkAdditionTable(addition: Addition, drinkId: ID)(implicit connection: Connection): Try[Unit] = {
      managed(connection.prepareStatement("INSERT INTO order_drink_additions (orderDrinkId, additionId, additionCost) SELECT ?, Addition.additionId, Addition.cost FROM Addition WHERE Addition.addition = ?;"))
        .map(prepStatement => {
          prepStatement.setString(1, drinkId.toString)
          prepStatement.setString(2, addition)
          prepStatement.executeUpdate()
        })
        .tried
        .map(_ => ())
    }

    def calculateCost(orderId: OrderId)(implicit connection: Connection): Try[Int] = {
      val resultSet = for {
        prepStatement <- managed(connection.prepareStatement("SELECT drinkId, drinkCost, additionCost FROM OrderView WHERE orderId = ?;"))
        _ = prepStatement.setInt(1, orderId)
        resultSet <- managed(prepStatement.executeQuery())
      } yield resultSet

      def getResults(resultSet: ResultSet) = {
        Stream.continually(resultSet.next())
          .takeWhile(true ==)
          .flatMap(_ => {
            for {
              drinkId <- Option(resultSet.getString("drinkId"))
              drinkCost = resultSet.getInt("drinkCost")
              additionCost = Option(resultSet.getInt("additionCost"))
            } yield (UUID.fromString(drinkId), drinkCost, additionCost)
          })
          .groupBy { case (drinkId, _, _) => drinkId }
          .flatMap { case (_, values) =>
            val drinkCost = values.headOption.map { case (_, cost, _) => cost }
            val additionCost = values.flatMap { case (_, _, cost) => cost }
            drinkCost.toStream #::: additionCost
          }
          .sum
      }

      resultSet.map(getResults).tried
    }
  }
}

package com.github.rvanheest.starbux.order

import java.sql.Connection

import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{Failure, Try}

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
        .map(_.groupBy { case (drinkId, _, _) => drinkId }
          .flatMap { case (_, values) =>
            val drinkCost = values.headOption.map { case (_, cost, _) => cost }
            val additionCost = values.map { case (_, _, cost) => cost }
            drinkCost.toSeq ++ additionCost
          }
          .sum)
    }
  }
}

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
package com.github.rvanheest.starbux.service.order

import java.sql.Connection

import com.github.rvanheest.starbux.order._
import com.github.rvanheest.starbux.service.TestSupportFixture
import nl.knaw.dans.lib.error.CompositeException
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalamock.scalatest.MockFactory

import scala.util.{Failure, Success, Try}

class OrderManagementSpec extends TestSupportFixture
  with MockFactory
  with OrderManagementComponent
  with DatabaseComponent
  with DebugEnhancedLogging {

  implicit private val connection: Connection = mock[Connection]
  override val database: Database = mock[Database]
  override val orderManagement: OrderManagement = new OrderManagement {}

  private val add1 = "sugar"
  private val add2 = "milk"
  private val add3 = "honey"

  private val drink1 = Drink(drink = "coffee")
  private val drink2 = Drink(drink = "coffee", additions = List(add1))
  private val drink3 = Drink(drink = "coffee", additions = List(add1, add2))
  private val drink4 = Drink(drink = "tea", additions = List(add3))

  private val order1 = Order(Ordered, Set.empty)
  private val order2 = Order(Prepared, Set(drink1))
  private val order3 = Order(Payed, Set(drink2))
  private val order4 = Order(Served, Set(drink3, drink4))

  "addOrder" should "insert an empty order" in {
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (*, *, *) never()
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (*, *, *) never()

    orderManagement.addOrder(order1) should matchPattern { case Success(`orderId`) => }
  }

  it should "insert an order with one drink and no additions on it" in {
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Prepared, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink1, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (*, *, *) never()

    orderManagement.addOrder(order2) should matchPattern { case Success(`orderId`) => }
  }

  it should "insert an order with one drink and one addition on it" in {
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Payed, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink2, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (add1, drink2.id, *) once() returning Try { () }

    orderManagement.addOrder(order3) should matchPattern { case Success(`orderId`) => }
  }

  it should "insert an order with multiple drinks and additions on them" in {
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Served, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink3, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (add1, drink3.id, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (add2, drink3.id, *) once() returning Try { () }
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink4, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (add3, drink4.id, *) once() returning Try { () }

    orderManagement.addOrder(order4) should matchPattern { case Success(`orderId`) => }
  }

  it should "fail if the drink is unknown" in {
    val drink = Drink(drink = "cola")
    val invalidOrder = Order(Ordered, Set(drink))
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink, orderId, *) once() returning Failure(UnknownItemException("Unknown drink: cola"))
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (*, *, *) never()

    orderManagement.addOrder(invalidOrder) should matchPattern {
      case Failure(UnknownItemException("Unknown drink: cola")) =>
    }
  }

  it should "fail if multiple drinks are unknown" in {
    val drink1 = Drink(drink = "cola")
    val drink2 = Drink(drink = "ice tea")
    val invalidOrder = Order(Ordered, Set(drink1, drink2))
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink1, orderId, *) once() returning Failure(UnknownItemException("Unknown drink: cola"))
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink2, orderId, *) once() returning Failure(UnknownItemException("Unknown drink: ice tea"))
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (*, *, *) never()

    orderManagement.addOrder(invalidOrder) should matchPattern {
      case Failure(CompositeException(UnknownItemException("Unknown drink: cola") :: UnknownItemException("Unknown drink: ice tea") :: Nil)) =>
    }
  }

  it should "fail if the addition is unknown" in {
    val drink = Drink(drink = "coffee", additions = List("whiskey"))
    val invalidOrder = Order(Ordered, Set(drink))
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects ("whiskey", drink.id, *) once() returning Failure(UnknownItemException("Unknown addition: whiskey"))

    orderManagement.addOrder(invalidOrder) should matchPattern {
      case Failure(UnknownItemException("Unknown addition: whiskey")) =>
    }
  }

  it should "fail if multiple additions are unknown" in {
    val drink = Drink(drink = "coffee", additions = List("whiskey", "vodka"))
    val invalidOrder = Order(Ordered, Set(drink))
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects ("whiskey", drink.id, *) once() returning Failure(UnknownItemException("Unknown addition: whiskey"))
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects ("vodka", drink.id, *) once() returning Failure(UnknownItemException("Unknown addition: vodka"))

    orderManagement.addOrder(invalidOrder) should matchPattern {
      case Failure(CompositeException(UnknownItemException("Unknown addition: whiskey") :: UnknownItemException("Unknown addition: vodka") :: Nil)) =>
    }
  }

  it should "fail if both drink and addition are unknown" in {
    val drink = Drink(drink = "cola", additions = List("whiskey"))
    val invalidOrder = Order(Ordered, Set(drink))
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink, orderId, *) once() returning Failure(UnknownItemException("Unknown drink: cola"))
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects ("whiskey", drink.id, *) never()

    orderManagement.addOrder(invalidOrder) should matchPattern {
      case Failure(UnknownItemException("Unknown drink: cola")) =>
    }
  }

  it should "fail if some drinks and addition are unknown" in {
    val drink1 = Drink(drink = "coffee", additions = List("whiskey"))
    val drink2 = Drink(drink = "cola")
    val invalidOrder = Order(Ordered, Set(drink1, drink2))
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink1, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects ("whiskey", drink1.id, *) once() returning Failure(UnknownItemException("Unknown addition: whiskey"))
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink2, orderId, *) once() returning Failure(UnknownItemException("Unknown drink: cola"))
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (*, *, *) never()

    orderManagement.addOrder(invalidOrder) should matchPattern {
      case Failure(CompositeException(UnknownItemException("Unknown addition: whiskey") :: UnknownItemException("Unknown drink: cola") :: Nil)) =>
    }
  }

  "calculateCost" should "calculate the cost of an empty order" in {
    val orderId = 1
    (database.costView(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try {
      (drink1.id, 0, 0) :: Nil
    }

    orderManagement.calculateCost(orderId) should matchPattern { case Success(0) => }
  }

  it should "calculate the cost of an order with one drink and no additions on it" in {
    val orderId = 1
    (database.costView(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try {
      (drink1.id, 1, 0) :: Nil
    }

    orderManagement.calculateCost(orderId) should matchPattern { case Success(1) => }
  }

  it should "calculate the cost of an order with one drink and one addition on it" in {
    val orderId = 1
    (database.costView(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try {
      (drink2.id, 1, 1) :: Nil
    }

    orderManagement.calculateCost(orderId) should matchPattern { case Success(2) => }
  }

  it should "calculate the cost of an order with multiple drinks and additions on them" in {
    val orderId = 1
    (database.costView(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try {
      (drink3.id, 1, 1) :: (drink3.id, 1, 1) :: (drink4.id, 2, 2) :: Nil
    }

    orderManagement.calculateCost(orderId) should matchPattern { case Success(7) => }
  }

  it should "fail to calculate the cost of an order that does not exist" in {
    val orderId = 1
    (database.costView(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try { Nil }

    orderManagement.calculateCost(orderId) should matchPattern { case Failure(UnknownOrderException(`orderId`)) => }
  }

  "getOrder" should "fail if the database can't find any orders" in {
    val orderId = 1
    (database.getOrder(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try { Nil }

    orderManagement.getOrder(orderId) should matchPattern { case Failure(UnknownOrderException(`orderId`)) => }
  }

  it should "return an empty order" in {
    val orderId = 1
    (database.getOrder(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try {
      ("Ordered", None, "", None) :: Nil
    }

    inside(orderManagement.getOrder(orderId)) {
      case Success(Order(status, drinks)) =>
        status shouldBe Ordered
        drinks shouldBe empty
    }
  }

  it should "fail if the state is unknown" in {
    val orderId = 1
    (database.getOrder(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try {
      ("unknown-state", None, "", None) :: Nil
    }

    orderManagement.getOrder(orderId) should matchPattern { case Failure(UnknownOrderStateException(`orderId`)) => }
  }

  it should "group order lines based on the drinkId and return a complete order" in {
    val orderId = 1
    (database.getOrder(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try {
      ("Ordered", Some(drink3.id), drink3.drink, Some(add1)) ::
      ("Ordered", Some(drink3.id), drink3.drink, Some(add2)) ::
      ("Ordered", Some(drink4.id), drink4.drink, Some(add3)) :: Nil
    }

    inside(orderManagement.getOrder(orderId)) {
      case Success(Order(status, drinks)) =>
        status shouldBe Ordered
        drinks should (have size 2 and contain only(drink3, drink4))
    }
  }

  it should "filter out order lines without drinkId" in {
    val orderId = 1
    (database.getOrder(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try {
      ("Ordered", Some(drink3.id), drink3.drink, Some(add1)) ::
        ("Ordered", Some(drink3.id), drink3.drink, Some(add2)) ::
        ("Ordered", None, "foo", None) ::
        ("Ordered", Some(drink4.id), drink4.drink, Some(add3)) :: Nil
    }

    inside(orderManagement.getOrder(orderId)) {
      case Success(Order(status, drinks)) =>
        status shouldBe Ordered
        drinks should (have size 2 and contain only(drink3, drink4))
    }
  }
}

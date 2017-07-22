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

import com.github.rvanheest.starbux.order._
import com.github.rvanheest.starbux.service.{DatabaseFixture, TestSupportFixture}

import scala.collection.immutable.Stream.Empty
import scala.util.{Failure, Success}

class DatabaseSpec extends TestSupportFixture with DatabaseFixture with DatabaseComponent {
  val database: Database = new Database {}

  private val add1 = "sugar"
  private val add2 = "milk"
  private val add3 = "honey"

  private val drink1 = Drink(drink = "coffee")
  private val drink2 = Drink(drink = "coffee", additions = List(add1))
  private val drink3 = Drink(drink = "coffee", additions = List(add1, add2))
  private val drink4 = Drink(drink = "tea", additions = List(add3))

  private val order1 = Order(Ordered, List.empty)
  private val order2 = Order(Prepared, List(drink1))
  private val order3 = Order(Payed, List(drink2))
  private val order4 = Order(Served, List(drink3, drink4))

  "addToOrderTable" should "create a new order" in {
    val orderId = 1
    database.addToOrderTable(Ordered) should matchPattern { case Success(`orderId`) => }

    inspectOrderTable should (have size 1 and contain only((orderId, 1)))
  }

  "addToOrderDrinkTable" should "add a drink to the order" in {
    val orderId = 1
    database.addToOrderTable(Prepared) shouldBe a[Success[_]]
    database.addToOrderDrinkTable(drink1, orderId) shouldBe a[Success[_]]

    inspectOrderTable should (have size 1 and contain only((orderId, 2)))
    inspectOrderDrinkTable should (have size 1 and contain only((drink1.id, 1, 1, 1)))
  }

  it should "fail if the drink is unknown" in {
    val orderId = 1
    database.addToOrderTable(Prepared) shouldBe a[Success[_]]
    database.addToOrderDrinkTable(Drink(drink = "cola"), orderId) should matchPattern {
      case Failure(UnknownItemException("Unknown drink: cola")) =>
    }

    inspectOrderTable should (have size 1 and contain only((orderId, 2)))
    inspectOrderDrinkTable shouldBe empty
  }

  "addToOrderDrinkAdditionTable" should "add an addition to a drink" in {
    val orderId = 1
    database.addToOrderTable(Prepared) shouldBe a[Success[_]]
    database.addToOrderDrinkTable(drink2, orderId) shouldBe a[Success[_]]
    database.addToOrderDrinkAdditionTable(add1, drink2.id) shouldBe a[Success[_]]

    inspectOrderTable should (have size 1 and contain only((orderId, 2)))
    inspectOrderDrinkTable should (have size 1 and contain only((drink2.id, 1, 1, 1)))
    inspectOrderDrinkAdditionsTable should (have size 1 and contain only((drink2.id, 2, 1)))
  }

  it should "fail if the addition is unknown" in {
    val orderId = 1
    database.addToOrderTable(Prepared) shouldBe a[Success[_]]
    database.addToOrderDrinkTable(drink2, orderId) shouldBe a[Success[_]]
    database.addToOrderDrinkAdditionTable("whiskey", drink2.id) should matchPattern {
      case Failure(UnknownItemException("Unknown addition: whiskey")) =>
    }

    inspectOrderTable should (have size 1 and contain only((orderId, 2)))
    inspectOrderDrinkTable should (have size 1 and contain only((drink2.id, 1, 1, 1)))
    inspectOrderDrinkAdditionsTable shouldBe empty
  }

  "costView" should "list the costs of an empty order" in {
    val orderId = 1
    database.addToOrderTable(Ordered) should matchPattern { case Success(`orderId`) => }

    database.costView(orderId) should matchPattern { case Success(Empty) => }
  }

  it should "list the costs of an order with one drink and no additions on it" in {
    val orderId = 1
    database.addToOrderTable(Ordered) should matchPattern { case Success(`orderId`) => }
    database.addToOrderDrinkTable(drink1, orderId) shouldBe a[Success[_]]

    inside(database.costView(orderId)) {
      case Success(view) => view should (have size 1 and contain only((drink1.id, 1, 0)))
    }
  }

  it should "list the costs of an order with one drink and one addition on it" in {
    val orderId = 1
    database.addToOrderTable(Ordered) should matchPattern { case Success(`orderId`) => }
    database.addToOrderDrinkTable(drink2, orderId) shouldBe a[Success[_]]
    database.addToOrderDrinkAdditionTable(add1, drink2.id) shouldBe a[Success[_]]

    inside(database.costView(orderId)) {
      case Success(view) => view should (have size 1 and contain only((drink2.id, 1, 1)))
    }
  }

  it should "list the costs of an order with multiple drinks and additions on them" in {
    val orderId = 1
    database.addToOrderTable(Ordered) should matchPattern { case Success(`orderId`) => }
    database.addToOrderDrinkTable(drink3, orderId) shouldBe a[Success[_]]
    database.addToOrderDrinkAdditionTable(add1, drink3.id) shouldBe a[Success[_]]
    database.addToOrderDrinkAdditionTable(add2, drink3.id) shouldBe a[Success[_]]
    database.addToOrderDrinkTable(drink4, orderId) shouldBe a[Success[_]]
    database.addToOrderDrinkAdditionTable(add3, drink4.id) shouldBe a[Success[_]]

    inside(database.costView(orderId)) {
      case Success(view) => view should (have size 3 and contain inOrder (
        (drink3.id, 1, 1),
//        (drink3.id, 1, 1),
        (drink4.id, 2, 2)
      ))
    }
  }
}

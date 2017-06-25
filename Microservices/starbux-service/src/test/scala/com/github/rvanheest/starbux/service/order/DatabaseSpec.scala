package com.github.rvanheest.starbux.service.order

import com.github.rvanheest.starbux.order._
import com.github.rvanheest.starbux.service.DatabaseFixture

import scala.util.{ Failure, Success }

class DatabaseSpec extends DatabaseFixture with DatabaseComponent {
  import databaseAccess.doTransaction
  val database: Database = new Database {}

  val add1 = "sugar"
  val add2 = "milk"
  val add3 = "honey"

  val drink1 = Drink(drink = "coffee", addition = List.empty)
  val drink2 = Drink(drink = "coffee", addition = List(add1))
  val drink3 = Drink(drink = "coffee", addition = List(add1, add2))
  val drink4 = Drink(drink = "tea", addition = List(add3))

  val order1 = Order(Ordered, List.empty)
  val order2 = Order(Prepared, List(drink1))
  val order3 = Order(Payed, List(drink2))
  val order4 = Order(Served, List(drink3, drink4))

  "addOrder" should "insert an empty order" in {
    val expectedOrderId = 1
    doTransaction(implicit connection => database.addOrder(order1)) should matchPattern {
      case Success(`expectedOrderId`) =>
    }

    inspectOrderTable should (have size 1 and contain only((expectedOrderId, 1)))
  }

  it should "insert an order with one drink and no additions on it" in {
    val expectedOrderId = 1
    doTransaction(implicit connection => database.addOrder(order2)) should matchPattern {
      case Success(`expectedOrderId`) =>
    }

    inspectOrderTable should (have size 1 and contain only((expectedOrderId, 2)))
    inspectOrderDrinkTable should (have size 1 and contain only((drink1.id, 1, 1, 1)))
  }

  it should "insert an order with one drink and one addition on it" in {
    val expectedOrderId = 1
    doTransaction(implicit connection => database.addOrder(order3)) should matchPattern {
      case Success(`expectedOrderId`) =>
    }

    inspectOrderTable should (have size 1 and contain only((expectedOrderId, 3)))
    inspectOrderDrinkTable should (have size 1 and contain only((drink2.id, 1, 1, 1)))
    inspectOrderDrinkAdditionsTable should (have size 1 and contain only((drink2.id, 2, 1)))
  }

  it should "insert an order with multiple drinks and additions on them" in {
    val expectedOrderId = 1
    doTransaction(implicit connection => database.addOrder(order4)) should matchPattern {
      case Success(`expectedOrderId`) =>
    }

    inspectOrderTable should (have size 1 and contain only((expectedOrderId, 4)))
    inspectOrderDrinkTable should (have size 2 and contain only((drink3.id, 1, 1, 1), (drink4.id, 1, 2, 2)))
    inspectOrderDrinkAdditionsTable should (have size 3 and contain only((drink3.id, 2, 1), (drink3.id, 1, 1), (drink4.id, 3, 2)))
  }

  it should "fail if the drink is unknown" in {
    val invalidOrder = Order(Ordered, List(Drink(drink = "cola", addition = List.empty)))
    doTransaction(implicit connection => database.addOrder(invalidOrder)) should matchPattern {
      case Failure(DatabaseException("Unknown drink: cola")) =>
    }

    inspectOrderTable shouldBe empty
    inspectOrderDrinkTable shouldBe empty
    inspectOrderDrinkAdditionsTable shouldBe empty
  }

  it should "fail if the addition is unknown" in {
    val invalidOrder = Order(Ordered, List(Drink(drink = "coffee", addition = List("whiskey"))))
    doTransaction(implicit connection => database.addOrder(invalidOrder)) should matchPattern {
      case Failure(DatabaseException("Unknown addition: whiskey")) =>
    }

    inspectOrderTable shouldBe empty
    inspectOrderDrinkTable shouldBe empty
    inspectOrderDrinkAdditionsTable shouldBe empty
  }

  "calculateCost" should "calculate the cost of an empty order" in {
    val expectedOrderId = 1
    database.addOrder(order1) should matchPattern { case Success(`expectedOrderId`) => }

    database.calculateCost(expectedOrderId) should matchPattern { case Success(0) => }
  }

  it should "calculate the cost of an order with one drink and no additions on it" in {
    val expectedOrderId = 1
    database.addOrder(order2) should matchPattern { case Success(`expectedOrderId`) => }

    database.calculateCost(expectedOrderId) should matchPattern { case Success(1) => }
  }

  it should "calculate the cost of an order with one drink and one addition on it" in {
    val expectedOrderId = 1
    database.addOrder(order3) should matchPattern { case Success(`expectedOrderId`) => }

    database.calculateCost(expectedOrderId) should matchPattern { case Success(2) => }
  }

  it should "calculate the cost of an order with multiple drinks and additions on them" in {
    val expectedOrderId = 1
    database.addOrder(order4) should matchPattern { case Success(`expectedOrderId`) => }

    database.calculateCost(expectedOrderId) should matchPattern { case Success(7) => }
  }
}

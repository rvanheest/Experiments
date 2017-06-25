package com.github.rvanheest.starbux.service.service

import java.net.URL
import java.sql.Connection

import com.github.rvanheest.starbux.order.{ DatabaseComponent, Order }
import com.github.rvanheest.starbux.service.{ DatabaseFixture, OrderServletComponent }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

import scala.util.Success
import scala.xml.Utility

class OrderServletSpec extends DatabaseFixture
  with MockFactory
  with ScalatraSuite
  with OrderServletComponent
  with DatabaseComponent
  with DebugEnhancedLogging {

  override val database: Database = mock[Database]
  override val orderServlet: OrderServlet = new OrderServlet {
    val baseUrl: URL = new URL("http://localhost:8060/")
  }

  addServlet(orderServlet, "/*")

  "get" should "return the message that the service is running" in {
    get("/") {
      status shouldBe 200
      body shouldBe "Starbux Coffee Service running"
    }
  }

  "post" should "send an order and receive a response with the correct cost and follow-up url" in {
    val input =
      <order>
        <drink>
          <name>coffee</name>
          <addition>sugar</addition>
          <addition>milk</addition>
        </drink>
        <drink>
          <name>thee</name>
          <addition>honey</addition>
        </drink>
      </order>
    val orderId = 1
    val cost = 25
    val output =
      <order>
        <drink>
          <name>coffee</name>
          <addition>sugar</addition>
          <addition>milk</addition>
        </drink>
        <drink>
          <name>thee</name>
          <addition>honey</addition>
        </drink>
        <cost>{cost}</cost>
        <next rel="http://localhost:8060/payment"
              uri="http://localhost:8060/payment/order/1"
              type="application/xml"/>
      </order>

    (database.addOrder(_: Order)(_: Connection)) expects (*, *) once() returning Success(orderId)
    (database.calculateCost(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Success(cost)

    post("/", "order" -> Utility.trim(input).toString()) {
      status shouldBe 201
      header should contain ("Location" -> "http://localhost:8060/order/1")
      body shouldBe Utility.trim(output).toString()
    }
  }
}

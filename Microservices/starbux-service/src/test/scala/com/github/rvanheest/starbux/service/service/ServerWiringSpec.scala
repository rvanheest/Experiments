package com.github.rvanheest.starbux.service.service

import com.github.rvanheest.starbux.order.DatabaseComponent
import com.github.rvanheest.starbux.service.{ DatabaseFixture, PropertiesSupportFixture, ServerTestSupportFixture, ServerWiring }
import org.scalatest.OneInstancePerTest

import scala.util.Success
import scala.xml.Utility

class ServerWiringSpec extends PropertiesSupportFixture with DatabaseFixture with ServerTestSupportFixture with OneInstancePerTest
  with ServerWiring
  with DatabaseComponent {

  val database: Database = new Database {}

  override def beforeEach(): Unit = {
    super.beforeEach()
    server.start() shouldBe a[Success[_]]
  }

  override def afterEach(): Unit = {
    server.stop() shouldBe a[Success[_]]
    server.destroy() shouldBe a[Success[_]]
    super.afterEach()
  }

  "serverPort" should "have the correct value based on the properties" in {
    server.serverPort shouldBe 20000
  }

  "get" should "indicate that the service is up and running" in {
    callService()() shouldBe successful
  }

  "post order" should "order a single drink without addition" in {
    val input =
      <order>
        <drink>
          <name>coffee</name>
        </drink>
      </order>

    val output =
      <order>
        <drink>
          <name>coffee</name>
        </drink>
        <cost>1</cost>
        <next type="application/xml"
              uri="http://localhost:20000/payment/order/1"
              rel="http://localhost:20000/payment"/>
      </order>

    order(input) shouldBe Utility.trim(output)
  }

  it should "order a single drink with additions" in {
    val input =
      <order>
        <drink>
          <name>coffee</name>
          <addition>sugar</addition>
          <addition>milk</addition>
        </drink>
      </order>

    val output =
      <order>
        <drink>
          <name>coffee</name>
          <addition>sugar</addition>
          <addition>milk</addition>
        </drink>
        <cost>3</cost>
        <next type="application/xml"
              uri="http://localhost:20000/payment/order/1"
              rel="http://localhost:20000/payment"/>
      </order>

    order(input) shouldBe Utility.trim(output)
  }

  it should "order multiple drinks with additions" in {
    val input =
      <order>
        <drink>
          <name>coffee</name>
          <addition>sugar</addition>
          <addition>milk</addition>
        </drink>
        <drink>
          <name>tea</name>
          <addition>honey</addition>
        </drink>
      </order>

    val output =
      <order>
        <drink>
          <name>coffee</name>
          <addition>sugar</addition>
          <addition>milk</addition>
        </drink>
        <drink>
          <name>tea</name>
          <addition>honey</addition>
        </drink>
        <cost>7</cost>
        <next type="application/xml"
              uri="http://localhost:20000/payment/order/1"
              rel="http://localhost:20000/payment"/>
      </order>

    order(input) shouldBe Utility.trim(output)
  }

  it should "order a drink that StarBux doesn't have" in {
    val input =
      <order>
        <drink>
          <name>cola</name>
        </drink>
      </order>

    failing(input) shouldBe "Unknown drink: cola"
  }
}

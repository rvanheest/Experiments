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

  "post order" should "order nothing" in {
    val input = <order/>

    failing(input) shouldBe (400, "The order did not contain any drinks")
  }

  it should "order a single drink without addition" in {
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

    order(input) shouldBe (201, Utility.trim(output))
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

    order(input) shouldBe (201, Utility.trim(output))
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

    order(input) shouldBe (201, Utility.trim(output))
  }

  it should "order a drink that StarBux doesn't have" in {
    val input =
      <order>
        <drink>
          <name>cola</name>
        </drink>
      </order>

    failing(input) shouldBe (400, "Unknown drink: cola")
  }

  it should "order multiple drinks that StarBux doesn't have" in {
    val input =
      <order>
        <drink>
          <name>cola</name>
        </drink>
        <drink>
          <name>ice tea</name>
        </drink>
      </order>

    failing(input) shouldBe (400, "Unknown drink: cola\nUnknown drink: ice tea")
  }

  it should "order an addition that StarBux doesn't have" in {
    val input =
      <order>
        <drink>
          <name>coffee</name>
          <addition>whiskey</addition>
        </drink>
      </order>

    failing(input) shouldBe (400, "Unknown addition: whiskey")
  }

  it should "order additions that StarBux doesn't have" in {
    val input =
      <order>
        <drink>
          <name>coffee</name>
          <addition>whiskey</addition>
          <addition>vodka</addition>
        </drink>
      </order>

    failing(input) shouldBe (400, "Unknown addition: whiskey\nUnknown addition: vodka")
  }

  it should "order a drink and addition that StarBux doesn't have" in {
    val input =
      <order>
        <drink>
          <name>cola</name>
          <addition>whiskey</addition>
        </drink>
      </order>

    failing(input) shouldBe (400, "Unknown drink: cola")
  }

  it should "order drinks and additions for which StarBux doesn't have some of them" in {
    val input =
      <order>
        <drink>
          <name>coffee</name>
          <addition>whiskey</addition>
        </drink>
        <drink>
          <name>cola</name>
        </drink>
      </order>

    failing(input) shouldBe (400, "Unknown addition: whiskey\nUnknown drink: cola")
  }
}

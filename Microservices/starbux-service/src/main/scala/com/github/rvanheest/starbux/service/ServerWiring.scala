package com.github.rvanheest.starbux.service

import java.net.URL

import com.github.rvanheest.starbux.order.DatabaseComponent
import com.github.rvanheest.starbux.{ DatabaseAccessComponent, PropertiesComponent }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

trait ServerWiring extends OrderServletComponent with StarBuxServletMounterComponent with StarBuxServerComponent {
  this: DatabaseAccessComponent with DatabaseComponent with PropertiesComponent with DebugEnhancedLogging =>

  private val port = properties.properties.getInt("service.daemon.http.port")

  override val orderServlet: OrderServlet = new OrderServlet {
    override val baseUrl: URL = new URL(s"http://localhost:$port/")
  }
  override val mounter: ServletMounter = new ServletMounter {}
  override val server: StarBuxServer = new StarBuxServer(port)
}

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

/**
 * Copyright (C) 2017 Richard van Heest (richard.v.heest@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.github.rvanheest.starbux

import java.nio.file.Paths

import com.github.rvanheest.starbux.service.{ OrderServletComponent, StarBuxServerComponent, StarBuxServletMounterComponent }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

object StarBuxWiring extends StarBuxServerComponent
  with StarBuxServletMounterComponent
  with OrderServletComponent
  with PropertiesComponent
  with DebugEnhancedLogging {

  private lazy val home = Paths.get(System.getProperty("app.home"))

  override lazy val properties: GeneralProperties = GeneralProperties(home)
  private val port = properties.properties.getInt("service.daemon.http.port")
  override lazy val orderServlet: StarBuxServlet = new StarBuxServlet {
    val serverPort: Int = port
  }
  override lazy val mounter: ServletMounter = new ServletMounter {}
  override lazy val server: StarBuxServer = new StarBuxServer(port)
}

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
package com.github.rvanheest.starbux.client.service

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.ServletContextHandler
import org.scalatra.LifeCycle
import org.scalatra.servlet.ScalatraListener

import scala.util.Try

trait ClientServerComponent {
  this: ClientServletMounterComponent with DebugEnhancedLogging =>

  val server: StarBuxServer

  class StarBuxServer(serverPort: Int) {
    private val server = new Server(serverPort)

    new ServletContextHandler(ServletContextHandler.NO_SESSIONS) {
      addEventListener(new ScalatraListener() {
        override def probeForCycleClass(classLoader: ClassLoader): (String, LifeCycle) = {
          (mounter.getClass.getSimpleName, mounter)
        }
      })
      server.setHandler(this)
    }
    logger.info(s"HTTP port is $serverPort")

    def start(): Try[Unit] = Try {
      logger.info("Starting StarBux Client server...")
      server.start()
    }

    def stop(): Try[Unit] = Try {
      logger.info("Stopping StarBux Client server...")
      server.stop()
    }

    def destroy(): Try[Unit] = Try {
      server.destroy()
      logger.info("StarBux Client server stopped.")
    }
  }
}

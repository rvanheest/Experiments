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
package com.github.rvanheest.starbux.client

import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.daemon.{ Daemon, DaemonContext }

import scala.util.control.NonFatal

class ClientDaemon extends Daemon with DebugEnhancedLogging {

  import ClientWiring._

  override def init(context: DaemonContext): Unit = {
    logger.info("Initializing service ...")

    // nothing to do

    logger.info("Service initialized.")
  }

  override def start(): Unit = {
    logger.info("Starting service ...")
//    databaseAccess.initConnectionPool()
//      .flatMap(_ => server.start())
    server.start()
      .doIfSuccess(_ => logger.info("Service started."))
      .doIfFailure {
        case NonFatal(e) => logger.error(s"Service startup failed: ${ e.getMessage }", e)
      }
      .getOrRecover(throw _)
  }

  override def stop(): Unit = {
    logger.info("Stopping service ...")
//    databaseAccess.closeConnectionPool()
//      .flatMap(_ => server.stop())
    server.stop()
      .doIfSuccess(_ => logger.info("Cleaning up ..."))
      .doIfFailure {
        case NonFatal(e) => logger.error(s"Service stop failed: ${ e.getMessage }", e)
      }
      .getOrRecover(throw _)
  }

  override def destroy(): Unit = {
    server.destroy()
      .doIfSuccess(_ => logger.info("Service stopped."))
      .doIfFailure {
        case e => logger.error(s"Service destroy failed: ${ e.getMessage }", e)
      }
      .getOrRecover(throw _)
  }
}

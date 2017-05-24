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
package com.github.rvanheest.starbux.client.cmd

import com.github.rvanheest.starbux.client.logic.OrdererComponent
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.language.reflectiveCalls
import scala.util.control.NonFatal
import scala.util.{ Failure, Try }
import scala.xml.PrettyPrinter

trait CommandLineRunComponent {
  this: CommandLineInterfaceComponent
    with OrdererComponent
    with DebugEnhancedLogging =>

  val run: CommandLineRun

  trait CommandLineRun {
    type FeedbackMessage = String

    def run(): FeedbackMessage = {
      debug("Starting command line interface")
      cli.verify()

      val result: Try[String] = cli.subcommand match {
        case Some(cmd @ cli.order) =>
          logger.info("order a drink")
          orderer.order(cmd.drink()).map(elem => "\n" + new PrettyPrinter(160, 2).format(elem))
        case None => ???
        case _ => Failure(new IllegalArgumentException(s"Unknown command: ${ cli.subcommands }"))
      }


      result
        .map(msg => s"OK: $msg")
        .doIfFailure {
          case NonFatal(e) => logger.error(e.getMessage, e)
        }
        .getOrRecover(e => s"FAILED: ${ e.getMessage }")
    }
  }
}

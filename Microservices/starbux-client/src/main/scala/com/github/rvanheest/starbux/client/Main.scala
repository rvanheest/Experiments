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
package com.github.rvanheest.starbux.client

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

object Main extends DebugEnhancedLogging {

  def main(args: Array[String]): Unit = {
    logger.debug("Starting StarBux client CLI")

    val client = new ClientWiring(args)
    println(client.run.run())
  }
}

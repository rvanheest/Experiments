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

import com.github.rvanheest.starbux.client.cmd.{ CommandLineInterfaceComponent, CommandLineRunComponent }
import com.github.rvanheest.starbux.client.logic.LogicWiring
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

class ClientWiring(params: Seq[String]) extends CommandLineInterfaceComponent
  with CommandLineRunComponent
  with LogicWiring
  with ConfigurationComponent
  with DebugEnhancedLogging {

  override lazy val configuration: Configuration = Configuration()
  override lazy val cli: CommandLineInterface = CommandLineInterface(params)
  override lazy val run: CommandLineRun = new CommandLineRun {}
}

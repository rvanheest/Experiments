#!/usr/bin/env bash
#
# Copyright (C) 2017 Richard van Heest (richard.v.heest@gmail.com)
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#         http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#


ARGS=$@
APPHOME=home

mvn exec:java -Dapp.home=$APPHOME \
              -Dexec.args="run-service" \
              -Dlogback.configurationFile=$APPHOME/cfg/logback-service.xml \
#              -Dlogback.statusListenerClass=ch.qos.logback.core.status.OnConsoleStatusListener

# Uncomment the last line if you need to examine the logback initialization process.
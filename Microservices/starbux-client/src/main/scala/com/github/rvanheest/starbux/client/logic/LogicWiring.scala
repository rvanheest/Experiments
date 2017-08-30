package com.github.rvanheest.starbux.client.logic

import java.net.URI

import com.github.rvanheest.starbux.client.ConfigurationComponent
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

trait LogicWiring extends HttpConnectionComponent with OrdererComponent {
  this: ConfigurationComponent with DebugEnhancedLogging =>

  override lazy val httpConnection: HttpConnection = new HttpConnection {
    private val key = "client.service.baseUrl"
    private val value = configuration.properties.getString(key)

    if (!value.endsWith("/")) {
      logger.warn(s"adding a '/' to the end of property '$key'")
      configuration.properties.setProperty(key, value + "/")
      configuration.properties.save()
    }

    val baseUri: URI = new URI(configuration.properties.getString(key))
  }
  override lazy val orderer: Orderer = new Orderer {}
}

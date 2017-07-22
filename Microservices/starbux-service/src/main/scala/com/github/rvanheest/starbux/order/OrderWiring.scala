package com.github.rvanheest.starbux.order

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

trait OrderWiring extends OrderManagementComponent with DatabaseComponent {
  this: DebugEnhancedLogging =>

  override val database: Database = new Database {}
  override val orderManagement: OrderManagement = new OrderManagement {}
}

trait ConfigurationComponent {

  val configuration: Configuration

  trait Configuration {
    def value: String
  }

  class DefaultConfiguration extends Configuration {
    val value = "production"
  }
  class TestConfiguration extends Configuration {
    val value = "test"
  }
}

trait AComponent {
  this: ConfigurationComponent =>

  val a: A

  trait A {
    val value = s"a-${configuration.value}"
  }
}

trait BComponent {
  this: ConfigurationComponent with AComponent =>

  val b: B

  trait B {
    val value = s"${a.value}-b-${configuration.value}"
  }
}

trait Components extends ConfigurationComponent
  with AComponent
  with BComponent

object Registry extends Components {
  val configuration: Configuration = new DefaultConfiguration
  val a: A = new A {}
  val b: B = new B {}
}

object TestRegistry extends Components {
  val configuration: Configuration = new TestConfiguration
  val a: A = new A {}
  val b: B = new B {}
}

Registry.b.value

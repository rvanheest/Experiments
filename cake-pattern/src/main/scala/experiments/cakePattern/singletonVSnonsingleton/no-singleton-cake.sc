import java.util.UUID

trait ConfigurationComponent {

  // singleton, so an access point
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
  this: ConfigurationComponent with CComponent =>

  // singleton, so an access point
  val a: A

  trait A {
    val c = new C {}
    val value = s"a-${ configuration.value }-${ c.value }"
  }
}

trait BComponent {
  this: ConfigurationComponent with AComponent with CComponent =>

  // singleton, so an access point
  val b: B

  trait B {
    val c = new C {}
    val value = s"${ a.value }-b-${ configuration.value }-${ c.value }"
  }
}

trait CComponent {
  this: ConfigurationComponent =>

  // not a singleton, so no access point
  // rather a local value in the dependent components

  trait C {
    val value = s"c-${ configuration.value }-${ UUID.randomUUID().toString.substring(1, 5) }"
  }
}

trait Components extends ConfigurationComponent
  with AComponent
  with BComponent
  with CComponent

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

trait Configuration {
  def value: String
}

class DefaultConfiguration extends Configuration {
  val value = "production"
}

class TestingConfiguration extends Configuration {
  val value = "test"
}

class A(configuration: Configuration) {
  val value: String = "a-" + configuration.value
}

class B(configuration: Configuration, a: A) {
  val value: String = a.value + "-b-" + configuration.value
}

val configuration = new DefaultConfiguration
val a = new A(configuration)
val b = new B(configuration, a)

b.value

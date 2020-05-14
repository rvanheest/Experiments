import java.util.UUID

trait Configuration {
  def value: String
}

class DefaultConfiguration extends Configuration {
  val value = "production"
}
class TestingConfiguration extends Configuration {
  val value = "test"
}

class A(configuration: Configuration, c: C) {
  val value = s"a-${ configuration.value }-${ c.value }"
}

class B(configuration: Configuration, a: A, c: C) {
  val value = s"${ a.value }-b-${ configuration.value }-${ c.value }"
}

class C(configuration: Configuration) {
  val value = s"c-${ configuration.value }-${ UUID.randomUUID().toString.substring(1, 5) }"
}

val configuration = new DefaultConfiguration
val a = new A(configuration, new C(configuration))
val b = new B(configuration, a, new C(configuration))

b.value

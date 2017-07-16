package monadics.test.laws

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

trait LawSpec extends PropSpec with PropertyChecks with Matchers {

  val name: String
}

package tutorial.jquery

import org.scalajs.jquery.jQuery

import scala.scalajs.js.annotation.JSExportTopLevel

object JQueryApp {

  @JSExportTopLevel("buttonClick")
  def buttonClick(): Unit = {
    jQuery("#click-results").append("<p>You clicked the button!</p>")
  }

  def main(args: Array[String]): Unit = {
  }
}

package tutorial.jquery

import org.scalajs.jquery.jQuery

object JQueryApp {

  def setupUI(): Unit = {
    jQuery("#click-me-button").click(() => buttonClick())
    jQuery("body").append("<p>Hello World</p>")
  }

  def buttonClick(): Unit = {
    jQuery("body").append("<p>You clicked the button!</p>")
  }

//  def main(args: Array[String]): Unit = {
//    jQuery(() => setupUI())
//  }
}

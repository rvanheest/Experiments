package tutorial.rx

import org.scalajs.jquery.jQuery
import rx._
import scalatags.JsDom.all._

object RxApp {

  implicit val ctx: Ctx.Owner = Ctx.Owner.safe()

  def setupUI(): Unit = {
    val body = jQuery("body")
    val header = h1(color := "red", textAlign := "center")("Hello World").render
    val b = button("Click me!").render
    body.append(header, b)

    val click: Var[Int] = Var(0)
    Rx {
      if (click() > 0)
        body.append(p("You clicked the button!").render)
    }
    b.onclick = _ => click() = click.now + 1
  }

//  def main(args: Array[String]): Unit = {
//    jQuery(() => setupUI())
//  }
}

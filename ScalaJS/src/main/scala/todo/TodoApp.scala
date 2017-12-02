package todo

import monix.execution.Scheduler.Implicits.global
import org.scalajs.jquery.{ JQuery, jQuery }

import scalatags.JsDom.all._

object TodoApp {

  def setupUI(root: JQuery): Unit = {
    val input = new UI.Input
    val list = new UI.TodoList

    input.textfieldKeyEvents
      .filter(_.key.toLowerCase == "enter")
      .foreach(_ => input.clickSubmit())

    input.textfieldEvents
      .withLatestFrom(input.submitEvents) { case (text, _) => text }
      .filter(_.trim.nonEmpty)
      .foreach(text => {
        list.append(listItem(text))
        input.text = ""
      })

    root.append(
      h1("TODO list").render,
      input(),
      list(),
    )
  }

  def listItem(text: String): UI.Todo = {
    val item = new UI.Todo(text)

    item.clickEvents
      .zipWithIndex
      .foreach {
        case (_, index) if index % 2 == 0 => item.setDone()
        case (_, _) => item.setNotDone()
      }

    item
  }

  def main(args: Array[String]): Unit = {
    jQuery(() => setupUI(jQuery("body")))
  }
}

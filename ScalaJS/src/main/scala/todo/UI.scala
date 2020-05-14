package todo

import monix.reactive.Observable
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.html._
import org.scalajs.dom.raw.KeyboardEvent

import scalatags.JsDom.all._

object UI {

  class Input {
    private val textfield = input(`type` := "text", name := "item", placeholder := "TODO item").render
    private val submit = button(`type` := "submit", name := "add")("Add").render
    private val content = div(textfield, submit).render

    def apply(): Div = content

    def textfieldKeyEvents: Observable[KeyboardEvent] = textfield.keyupEvents

    def textfieldEvents: Observable[String] = textfieldKeyEvents.map(_ => text)

    def submitEvents: Observable[MouseEvent] = submit.clickEvents

    def clickSubmit(): Unit = submit.click()

    def text: String = textfield.value

    def text_=(text: String) : Unit = textfield.value = text
  }

  class Todo(text: String) {
    private val listItem = li(text).render

    def apply(): LI = listItem

    def setDone(): Unit = listItem.style.textDecoration = "line-through"

    def setNotDone(): Unit = listItem.style.textDecoration = ""

    def clickEvents: Observable[MouseEvent] = listItem.clickEvents
  }

  class TodoList {
    private val uList = ul().render

    def apply(): UList = uList

    def append(todo: Todo): Unit = uList.appendChild(todo())
  }
}

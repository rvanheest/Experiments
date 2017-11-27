package todo

import org.scalajs.dom.html.LI
import org.scalajs.jquery.{ JQuery, jQuery }

import scalatags.JsDom.all._

object TodoApp {

  def setupUI(root: JQuery): Unit = {
    val header = h1("TODO list").render
    val tf = input(`type` := "text", name := "item", placeholder := "TODO item").render
    val submit = button(`type` := "submit", name := "add")("Add").render
    val list = ul().render

    tf.onkeyup = e => { if (e.key == "Enter") submit.click() }
    submit.onclick = _ => {
      list.appendChild(listItem(tf.value))
      tf.value = ""
    }

    root.append(
      header,
      tf,
      submit,
      list
    )
  }

  def listItem(text: String): LI = {
    val item = li(text).render

    item.onclick = _ => {
      item.style.textDecoration = item.style.textDecoration match {
        case "line-through" => "none"
        case _ => "line-through"
      }
    }

    item
  }

  def main(args: Array[String]): Unit = {
    jQuery(() => setupUI(jQuery("body")))
  }
}

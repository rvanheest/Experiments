package tutorial.dom

import org.scalajs.dom.{ Node, document }

import scala.scalajs.js.annotation.JSExportTopLevel

object DomApp {
  def header(text: String): Node = {
    val h1 = document.createElement("h1")
    h1.textContent = text
    h1
  }

  def paragraph(text: String): Node = {
    val p = document.createElement("p")
    p.appendChild(document.createTextNode(text))
    p
  }

  def link(text: String, url: String): Node = {
    val a = document.createElement("a")
    a.appendChild(document.createTextNode(text))
    a.setAttribute("href", url.toString)
    a
  }

  @JSExportTopLevel("addClickedMessage")
  def buttonClick(): Unit = {
    document.body.appendChild(paragraph("You clicked the button!"))
  }

//  def main(args: Array[String]): Unit = {
//    val targetNode = document.body
//
//    targetNode.appendChild(header("Header1"))
//    targetNode.appendChild(paragraph("Hello World"))
//    targetNode.appendChild(link("link", "http://www.google.com"))
//  }
}

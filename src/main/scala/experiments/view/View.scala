package experiments.view

abstract class View[Original, View] {

  implicit def in(original: Original): View
  implicit def out(view: View): Original
}

package todo

object Model {

  case class Todo(text: String, done: Boolean = false) {
    def toggleDone: Todo = copy(done = !done)
  }

  case class TodoList(list: List[Todo]) {
    def +=(todo: Todo): TodoList = copy(list = list :+ todo)
  }
}

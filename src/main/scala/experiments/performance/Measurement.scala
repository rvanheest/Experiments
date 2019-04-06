package experiments.performance

trait Measurement {
  def timed[A](name: String)(task: => A): Unit = {
    println(s"start $name")
    val start = System.nanoTime()
    task
    val end = System.nanoTime()
    println(s"$name took ${ end - start } nanoseconds (${ (end - start) / 1E9 } seconds)")
    println
  }
}

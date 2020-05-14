package experiments.action

import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

trait Action2[-A, +T] { self =>

  def checkPreconditions: Try[Unit]

  def execute(a: A): Try[T]

  def rollback(): Try[Unit]

  def run(a: A): Try[T] = {
    def recoverPreconditions(t: Throwable) = {
      Failure(PreconditionsFailedException(
        report = generateReport(
          header = "Precondition failures:",
          throwable = t,
          footer = "Due to these errors in the preconditions, nothing was done."),
        cause = t))
    }

    def recoverRun(t: Throwable) = {
      Failure(ActionRunFailedException(
        report = generateReport(
          header = "Errors during processing:",
          throwable = t,
          footer = "The actions that were already performed, were rolled back."),
        cause = t
      ))
    }

    for {
      _ <- checkPreconditions.recoverWith { case NonFatal(e) => recoverPreconditions(e) }
      t <- execute(a).recoverWith {
        case NonFatal(e) =>
          rollback() match {
            case Success(_) => recoverRun(e)
            case Failure(e2) => recoverRun(CompositeException(e :: e2 :: Nil))
          }
      }
    } yield t
  }

  private def generateReport(header: String = "", throwable: Throwable, footer: String = ""): String = {
    def report(throwable: Throwable): Seq[String] = {
      List(throwable)
        .flatMap {
          case es: CompositeException => es.throwables
          case e => Seq(e)
        }
        .map {
          case ActionException(-1, msg, _) => s" - cmd line: $msg"
          case ActionException(row, msg, _) => s" - row $row: $msg"
          case NonFatal(ex) => s" - unexpected error: ${ ex.getMessage }"
        }
    }

    header.toOption.fold("")(_ + "\n") +
      report(throwable).distinct.mkString("\n") +
      footer.toOption.fold("")("\n" + _)
  }

}
object Action2 {

  def identity[T]: Action2[T, T] = new Action2[T, T] {
    override def checkPreconditions: Try[Unit] = Success(())

    override def execute(t: T): Try[T] = Try { t }

    override def rollback(): Try[Unit] = Success(())
  }

  def apply[A, T](pre: () => Try[Unit] = () => Success(()),
               exe: A => Try[T],
               rb: () => Try[Unit] = () => Success(())): Action2[A, T] = new Action2[A, T] {
    override def checkPreconditions: Try[Unit] = pre()

    override def execute(a: A): Try[T] = exe(a)

    override def rollback(): Try[Unit] = rb()
  }

  def apply[A, T](execute: A => Try[T]): Action2[A, T] = Action2(exe = execute)
}

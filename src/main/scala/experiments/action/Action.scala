package experiments.action

import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

case class PreconditionsFailedException(report: String, cause: Throwable = null) extends Exception(report)
case class ActionRunFailedException(report: String, cause: Throwable = null) extends Exception(report)
case class ActionException(row: Int, message: String, cause: Throwable = null) extends Exception(message, cause)

trait Action[T] { self =>

	def checkPreconditions: Try[Unit]

	def execute(): Try[T]

	def rollback(): Try[Unit]

	def run(): Try[T] = {
		for {
			_ <- checkPreconditions.recoverWith {
				case NonFatal(e) => Failure(PreconditionsFailedException(
					report = generateReport(
						header = "Precondition failures:",
						t = e,
						footer = "Due to these errors in the preconditions, nothing was done."),
					cause = e))
			}
			t <- execute().recoverWith {
				case NonFatal(e1) =>
					def runFailed(t: Throwable): Try[T] = {
						Failure(ActionRunFailedException(
							report = generateReport(
								header = "Errors in Multi-Deposit Instructions file:",
								t = t,
								footer = "The actions that were already performed, were rolled back."),
							cause = t
						))
					}

					rollback().flatMap(_ => runFailed(e1))
					  .recoverWith { case NonFatal(e2) => runFailed(CompositeException(List(e1, e2))) }
			}
		} yield t
	}

	private def generateReport(header: String = "", t: Throwable, footer: String = ""): String = {
		def toOption(s: String): Option[String] = if (s.trim.isEmpty) Option.empty else Option(s)

		def report(es: List[Throwable], rpt: String = ""): String = {
			es match {
				case Nil => rpt
				case ActionException(row, msg, _) :: xs => report(xs, s" - row $row: $msg" + rpt)
				case CompositeException(ths) :: xs => report(ths.toList ::: xs, rpt)
				case NonFatal(ex) :: xs => report(xs, s" - unexpected error: ${ex.getMessage}" + rpt)
			}
		}

		toOption(header).fold("")(_ + "\n") + report(List(t)) + toOption(footer).fold("")("\n" + _)
	}

	def applyLeft[S, R](other: Action[S])(implicit ev: T <:< (S => R)): Action[R] = combineWith(other)(_(_))

	def applyRight[S](other: Action[T => S]): Action[S] = combineWith(other)((t, f) => f(t))

	def combine(other: Action[Unit]): Action[T] = combineWith(other)((t, _) => t)

	def combineWith[S, R](other: Action[S])(f: (T, S) => R): Action[R] = new Action[R] {
		private lazy val executedActions = mutable.Stack[Action[_]]()

		override def checkPreconditions: Try[Unit] = {
			List(self, other).map(_.checkPreconditions).collectResults.map(_ => ())
		}

		override def execute(): Try[R] = {
			executedActions.push(self)
			self.execute()
				.doIfSuccess(_ => executedActions.push(other))
				.flatMap(t1 => other.execute().map(f(t1, _)))
		}

		override def rollback(): Try[Unit] = {
			Stream.continually(executedActions.isEmpty)
				.takeWhile(_ == true)
				.map(_ => executedActions.pop().rollback())
				.toList
				.collectResults
				.map(_ => ())
		}
	}
}

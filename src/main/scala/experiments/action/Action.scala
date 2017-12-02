package experiments.action

import scala.annotation.tailrec
import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

/*
  Note, this thing is an Applicative Functor and should therefore satisfy the corresponding laws:
    - fmap id = id
    - fmap (g . h) = (fmap g) . (fmap h)
    - pure id <*> v = v
    - pure f <*> pure x = pure (f x)								(Homomorphism)
    - u <*> pure y = pure ($ y) <*> u								(Interchange)
    - u <*> (v <*> w) = pure (.) <*> u <*> v <*> w	(Composition)
    - u *> v = pure (const id) <*> u <*> v
    - u <* v = pure const <*> u <*> v
    - fmap f x = pure f <*> x
 */
trait Action[T] { self =>

	def checkPreconditions: Try[Unit]

	def execute(): Try[T]

	def rollback(): Try[Unit]

	def run(): Try[T] = {
		def runFailed(t: Throwable): Try[T] = {
			Failure(ActionRunFailedException(
				report = generateReport(
					header = "Errors in Multi-Deposit Instructions file:",
					t = t,
					footer = "The actions that were already performed, were rolled back."),
				cause = t
			))
		}

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
				case e1@CompositeException(es) =>
					rollback() match {
						case Success(_) => runFailed(e1)
						case Failure(CompositeException(es2)) => runFailed(CompositeException(es ++ es2))
						case Failure(e2) => runFailed(CompositeException(es ++ List(e2)))
					}
				case NonFatal(e1) =>
					rollback() match {
						case Success(_) => runFailed(e1)
						case Failure(CompositeException(es2)) => runFailed(CompositeException(List(e1) ++ es2))
						case Failure(e2) => runFailed(CompositeException(List(e1, e2)))
					}
			}
		} yield t
	}

	private def generateReport(header: String = "", t: Throwable, footer: String = ""): String = {
		def toOption(s: String): Option[String] = if (s.trim.isEmpty) Option.empty else Option(s)

		@tailrec
		def report(es: List[Throwable], rpt: List[String] = Nil): List[String] = {
			es match {
				case Nil => rpt
				case ActionException(row, msg, _) :: xs => report(xs, s" - row $row: $msg" :: rpt)
				case CompositeException(ths) :: xs => report(ths.toList ::: xs, rpt)
				case NonFatal(ex) :: xs => report(xs, s" - unexpected error: ${ex.getMessage}" :: rpt)
			}
		}

		toOption(header).fold("")(_ + "\n") + report(List(t)).reverse.mkString("\n") + toOption(footer).fold("")("\n" + _)
	}

	// fmap
	def map[S](f: T => S): Action[S] = new Action[S] {
		override def checkPreconditions: Try[Unit] = self.checkPreconditions

		override def execute(): Try[S] = self.execute().map(f)

		override def rollback(): Try[Unit] = self.rollback()
	}

	/*
	  Note that this operator will change the order of operations in the 'execute' phase.
	  First 'other' is executed, then 'self'.
	  The order in the preconditions and rollback phases is preserved.
	 */
	// <*>
	def applyLeft[S, R](other: Action[S])(implicit ev: T <:< (S => R)): Action[R] = combineWith(other)((f, t) => f(t))

	// <**>
	def applyRight[S](other: Action[T => S]): Action[S] = combineWith(other)((t, f) => f(t))

	// <*
	def thenAnd[S](other: Action[S]): Action[T] = combineWith(other)((t, _) => t)

	// *>
	def andThen[S](other: Action[S]): Action[S] = combineWith(other)((_, s) => s)

	// liftA2
	def combineWith[S, R](other: Action[S])(f: (T, S) => R): Action[R] = new Action[R] {
		private var pastSelf = false

		override def run(): Try[R] = {
			pastSelf = false
			super.run()
		}

		override def checkPreconditions: Try[Unit] = {
			List(self, other).map(_.checkPreconditions).collectResults.map(_ => ())
		}

		override def execute(): Try[R] = {
			for {
				t <- self.execute()
				_ = pastSelf = true
				s <- other.execute()
			} yield f(t, s)
		}

		override def rollback(): Try[Unit] = {
			(if (pastSelf) List(other, self) else List(self))
				.map(_.rollback())
				.collectResults
				.map(_ => ())
		}
	}
}

object Action {

	def apply[A](pre: () => Try[Unit] = () => Success(()),
							 exe: () => Try[A],
							 rb: () => Try[Unit] = () => Success(())): Action[A] = new Action[A] {
		def checkPreconditions: Try[Unit] = pre()

		def execute(): Try[A] = exe()

		def rollback(): Try[Unit] = rb()
	}

	def from[A](a: A): Action[A] = Action(exe = () => Success(a))
}

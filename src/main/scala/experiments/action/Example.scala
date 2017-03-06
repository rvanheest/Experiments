package experiments.action
import scala.util.{Success, Try}

object Example extends App {

	val getOnce: Action[Int] = GetOnceAction()
	val dependend: Action[Int => Unit] = DependendAction()
	val unrelated: Action[Unit] = UnrelatedAction()

	val act1: Action[Unit] = getOnce.applyRight(dependend).andThen(unrelated)
	val act2: Action[Unit] = getOnce.andThen(unrelated).applyRight(dependend)
	val act3: Action[Unit] = dependend.applyLeft(getOnce).andThen(unrelated)
	val act4: Action[Unit] = unrelated.thenAnd(dependend).applyLeft(getOnce)

	println(act1.run())
	println
	println(act2.run())
	println
	println(act3.run())
	println()
	println(act4.run())

	case class GetOnceAction() extends Action[Int] {
		lazy val computeOnce: Int = {
			println(" - this is only computed once")
			42
		}

		override def checkPreconditions: Try[Unit] = {
			println("GetOnceAction precondition")
			Success(())
		}

		override def execute(): Try[Int] = {
			println("GetOnceAction execute")
			Success(computeOnce)
		}

		override def rollback(): Try[Unit] = {
			println("GetOnceAction rollback")
			Success(())
		}
	}

	case class DependendAction() extends Action[Int => Unit] {
		override def checkPreconditions: Try[Unit] = {
			println("DependendAction precondition")
			Success(())
		}

		override def execute(): Try[Int => Unit] = Try { (i: Int) =>
			println("DependendAction execute")
			println(s" - the value was: $i")
		}

		override def rollback(): Try[Unit] = {
			println("DependendAction rollback")
			Success(())
		}
	}

	case class UnrelatedAction() extends Action[Unit] {
		override def checkPreconditions: Try[Unit] = {
			println("UnrelatedAction precondition")
			Success(())
		}

		override def execute(): Try[Unit] = {
			println("UnrelatedAction execute")
			Success(())
		}

		override def rollback(): Try[Unit] = {
			println("UnrelatedAction rollback")
			Success(())
		}
	}
}

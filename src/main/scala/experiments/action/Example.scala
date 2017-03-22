package experiments.action
import scala.util.{ Failure, Success, Try }

object Example extends App {

	val getOnce: Action[Int] = GetOnceAction()
	val dependend: Action[Int => Unit] = DependendAction()
	val unrelated: Action[Unit] = UnrelatedAction()

	val act1: Action[Unit] = getOnce.applyRight(dependend).andThen(unrelated)
	val act2: Action[Unit] = getOnce.thenAnd(unrelated).applyRight(dependend)
	val act3: Action[Unit] = dependend.applyLeft(getOnce).andThen(unrelated)
	val act4: Action[Unit] = unrelated.andThen(dependend).applyLeft(getOnce)
	val act5: Action[Unit] = dependend.thenAnd(unrelated).applyLeft(getOnce)

//	println(act1.run())
//	println
//	println(act2.run())
//	println
	println(act3.run())
//	println
//	println(act4.run())
//	println
//	println(act5.run())

	case class GetOnceAction() extends Action[Int] {
		lazy val computeOnce: Int = {
			println(" - this is only computed once")
			42
		}

		override def checkPreconditions: Try[Unit] = {
			println("GetOnceAction precondition")
//			Failure(ActionException(1, "foo"))
			Success(())
		}

		override def execute(): Try[Int] = {
			println("GetOnceAction execute")
//			Failure(ActionException(1, "foo"))
			Success(computeOnce)
		}

		override def rollback(): Try[Unit] = {
			println("GetOnceAction rollback")
			Failure(ActionException(1, "foo undo"))
//			Success(())
		}
	}

	case class DependendAction() extends Action[Int => Unit] {
		override def checkPreconditions: Try[Unit] = {
			println("DependendAction precondition")
//			Failure(ActionException(2, "bar"))
			Success(())
		}

		override def execute(): Try[Int => Unit] = Try { (i: Int) =>
			println("DependendAction execute")
			println(s" - the value was: $i")
		}

		override def rollback(): Try[Unit] = {
			println("DependendAction rollback")
			Failure(ActionException(2, "bar undo"))
//			Success(())
		}
	}

	case class UnrelatedAction() extends Action[Unit] {
		override def checkPreconditions: Try[Unit] = {
			println("UnrelatedAction precondition")
//			Failure(ActionException(3, "baz"))
			Success(())
		}

		override def execute(): Try[Unit] = {
			println("UnrelatedAction execute")
			Failure(ActionException(3, "baz"))
//			Success(())
		}

		override def rollback(): Try[Unit] = {
			println("UnrelatedAction rollback")
			Failure(ActionException(3, "baz undo"))
//			Success(())
		}
	}
}

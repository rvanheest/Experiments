package experiments.action

import scala.util.{ Success, Try }

object Example2 extends App {

  val getOnce = GetOnceAction()
  val dependend = DependendAction()
  val unrelated = UnrelatedAction()

  val act1 = getOnce.concat(dependend).andThen(unrelated)
  val act2 = DependendAction().combine(DependendAction())(_ + _)

  println(act1.run())
  println
  println(act2.run(1))

  case class GetOnceAction() extends Action2[Unit, Int] {
    lazy val computeOnce: Int = {
      println(" - this is only computed once")
      42
    }

    def checkPreconditions: Try[Unit] = {
      println("GetOnceAction precondition")
//			Failure(ActionException(1, "foo"))
      Success(())
    }

    def execute(a: Unit): Try[Int] = {
      println("GetOnceAction execute")
//			Failure(ActionException(1, "foo"))
      Success(computeOnce)
    }

    def rollback(): Try[Unit] = {
      println("GetOnceAction rollback")
//      Failure(ActionException(1, "foo undo"))
			Success(())
    }
  }

  case class DependendAction() extends Action2[Int, Int] {
    def checkPreconditions: Try[Unit] = {
      println("DependendAction precondition")
//			Failure(ActionException(2, "bar"))
      Success(())
    }

    def execute(i: Int): Try[Int] = Try {
      println("DependendAction execute")
      println(s" - the value was: $i")
      i + 1
    }

    def rollback(): Try[Unit] = {
      println("DependendAction rollback")
//      Failure(ActionException(2, "bar undo"))
			Success(())
    }
  }

  case class UnrelatedAction() extends Action2[Unit, Unit] {
    override def checkPreconditions: Try[Unit] = {
      println("UnrelatedAction precondition")
//			Failure(ActionException(3, "baz"))
      Success(())
    }

    override def execute(u: Unit): Try[Unit] = {
      println("UnrelatedAction execute")
//      Failure(ActionException(3, "baz"))
			Success(())
    }

    override def rollback(): Try[Unit] = {
      println("UnrelatedAction rollback")
//      Failure(ActionException(3, "baz undo"))
			Success(())
    }
  }
}

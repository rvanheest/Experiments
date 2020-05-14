package experiments.codingChallenges

import rx.exceptions.CompositeException
import rx.lang.scala.{ Observable, Subscriber, Subscription }

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

object FailFastAndSlowStream extends App {

  case class SomeException(i: Int = SomeException.getNextId) extends Exception(s"some exception $i")
  object SomeException {
    var nextId = 1

    def getNextId: Int = {
      val nid = nextId
      nextId += 1
      nid
    }
  }
  case class FatalException() extends Exception("fatal exception")

  runCustomErrorCollectExceptions()
  runCustomErrorFailFast()

  /**
   * 3 - 4 - 5 - 6 - 7 - onError 2xSomeException
   */
  def runCustomErrorCollectExceptions(): Unit = {
    customObservable(Observable.just(3, 4, 2, 5, 2, 6, 7))
  }

  /**
   * 3 - 4 - 5 - onError SomeException + FatalException
   */
  def runCustomErrorFailFast(): Unit = {
    customObservable(Observable.just(3, 4, 2, 5, 1, 6, 2, 7))
  }

  def customObservable(input: Observable[Int]): Subscription = {
    input
      .lift(new FlatMapCollectSomeErrorsOperator[Int]({
        case 1 => Observable.error(FatalException())
        case 2 => Observable.error(SomeException())
        case i => Observable.just(i)
      }, {
        case SomeException(_) => true
        case _ => false
      }))
      .subscribe(
        i => println(s"onNext: $i"),
        {
          case e: CompositeException => println(e.getExceptions.asScala.map(e => s" - ${ e.getMessage }").mkString("onError:\n", "\n", ""))
          case e => println(s"onError: ${ e.getMessage }")
        },
        () => println("onCompleted")
      )
  }

  class FlatMapCollectSomeErrorsOperator[T](obsGenerator: T => Observable[T], collectError: Throwable => Boolean) extends (Subscriber[T] => Subscriber[T]) {
    override def apply(subscriber: Subscriber[T]): Subscriber[T] = {
      new Subscriber[T]() {
        private val collectedErrors = ListBuffer.empty[Throwable]

        override def onNext(value: T): Unit = {
          if (!isUnsubscribed) {
            this.add {
              obsGenerator(value).subscribe(
                t => if (!isUnsubscribed) subscriber.onNext(t),
                error => if (collectError(error)) collectedErrors += error
                         else if (collectedErrors.isEmpty) subscriber.onError(error)
                         else subscriber.onError(new CompositeException((collectedErrors += error).asJava))
              )
            }
          }
        }

        override def onError(error: Throwable): Unit = {
          subscriber.onError(error)

          if (!this.isUnsubscribed)
            this.unsubscribe()
        }

        override def onCompleted(): Unit = {
          if (collectedErrors.isEmpty)
            subscriber.onCompleted()
          else
            subscriber.onError(new CompositeException(collectedErrors.asJava))

          if (!this.isUnsubscribed)
            this.unsubscribe()
        }
      }
    }
  }
}

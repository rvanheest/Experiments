import monix.execution.Scheduler.Implicits.global
import monix.execution.cancelables.SingleAssignmentCancelable
import monix.execution.{ Ack, Cancelable }
import monix.reactive.Observable
import monix.reactive.OverflowStrategy.Unbounded
import org.scalajs.dom.raw.{ EventTarget, KeyboardEvent }
import org.scalajs.dom.{ Event, MouseEvent }

import scala.scalajs.js

package object todo {

  implicit class EventHandler(val elem: EventTarget) extends AnyVal {
    def handleEvent(event: String): Observable[Event] = {
      Observable.create(Unbounded)(subscriber => {
        val cancel = SingleAssignmentCancelable()
        val f: js.Function1[Event, Ack] = (e: Event) => {
          subscriber.onNext(e).syncOnStopOrFailure(_ => cancel.cancel())
        }

        elem.addEventListener(event, f)
        cancel := Cancelable(() => elem.removeEventListener(event, f))
      })
    }

    def keyupEvents: Observable[KeyboardEvent] = handleEvent("keyup").collect { case e: KeyboardEvent => e }

    def clickEvents: Observable[MouseEvent] = handleEvent("click").collect { case e: MouseEvent => e }
  }
}

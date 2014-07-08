package actors

import akka.actor._
import akka.routing._
import akka.event.Logging
import akka.pattern.pipe
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import models._
import scala.util.{ Try, Success, Failure }
import play.api.libs.json._

object TaskManager {
  type Params = JsObject
  case object GetTasks
  case class TaskNames(xs: Iterable[String])

  case class StartTask[T](analyzer: Analyzer[T, _], batch: TaskCoordinator.WorkBatch[T], params: Params = JsObject(Seq()))
  case object CouldNotStart
  case class Started(name: String)
  case class GetProgressFor(name: String)
  case class CancelTask(name: String)
  case class Canceled(name: String)
  case class Done(resultID: Long)

  case object TaskNotFound  
}

object TaskCoordinator {
  case object GetProgress
  case object Cancel
  case class Progress(name: String, amt: Double)

  case object NoWorkReceived

  case class WorkBatch[T](ts: Seq[T])
  case class Results[R](name: String, ts: Seq[Try[R]])
}

object TaskWorker {
  case class WorkUnit[T](i: Int, t: T)
  case class Result[R](i: Int, r: Try[R])
}

class TaskManager extends Actor {
  val log = Logging(context.system, this)
  var outside: Option[ActorRef] = None
  var finished = Map[String, Long]()

  def forwardToController[T: ClassTag](t: T) = {
    outside match {
      case Some(ref) => ref ! t
      case None =>
        log.error("No reference to controller!")
    }
  }

  def receive = {
    case TaskManager.GetTasks =>
      outside = Some(sender)
      outside.getOrElse(sender) ! TaskManager.TaskNames(context.children.map(_.path.name))
    case TaskManager.GetProgressFor(name) =>
      outside = Some(sender)
      if (finished.contains(name)) {
        val resultID = finished(name)
        forwardToController(TaskManager.Done(resultID))
      } else {
        val child = context.child(name)
        child match {
          case Some(ref) =>
            ref ! TaskCoordinator.GetProgress
          case None =>
            outside.getOrElse(sender) ! TaskManager.TaskNotFound
        }
      }
    case TaskManager.StartTask(analyzer, work, params) =>
      outside = Some(sender)
      if (work.ts.size == 0) forwardToController(TaskManager.CouldNotStart)
      val coordinator = context.actorOf(analyzer.actorProps(self, params))
      coordinator ! work
    case TaskManager.CancelTask(name) =>
      outside = Some(sender)
      val child = context.child(name)
      child match {
        case Some(ref) =>
          ref ! TaskCoordinator.Cancel
        case None =>
          outside.getOrElse(sender) ! TaskManager.TaskNotFound
      }
    case s: TaskManager.Started =>
      forwardToController(s)
    case p: TaskCoordinator.Progress =>
      forwardToController(p)
    case r: TaskCoordinator.Results[_] =>
      // TODO store results somehow
      finished += sender.path.name -> 1L
      forwardToController(r)
    case Terminated(child) =>
      forwardToController(TaskManager.Canceled(child.path.name))
  }
}
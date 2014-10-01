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

  sealed trait In
  sealed trait Out

  case object GetTasks extends In
  case class TaskNames(xs: Iterable[String]) extends Out

  case class StartTask[T](
    analyzer: Analyzer[T, _],
    batch: TaskCoordinator.WorkBatch[T],
    params: Params = Json.obj()) extends In
  case object CouldNotStart extends Out
  case class Started(name: String) extends Out

  case class GetCoordinator(name: String) extends In
  case class Coordinator(ref: ActorRef) extends Out

  case class CoordinatorFinished(name: String, status: TaskCoordinator.Status) extends In
}

object TaskCoordinator {
  type ResultID = Long
  sealed trait In
  sealed trait Out

  case object GetProgress extends In
  case object Cancel extends In

  case class Progress(amt: Double) extends Out

  sealed trait Status extends Out

  case object Failed extends Status
  case object Canceled extends Status
  case class Done(resultID: ResultID) extends Status
  case object NoWorkReceived extends Status
  case object NotFound extends Status

  case class WorkBatch[T](ts: Seq[T]) extends In
  case class Results[R](name: String, ts: Seq[Try[R]]) extends Out
}

object TaskWorker {
  sealed trait In
  sealed trait Out

  case class WorkUnit[T](i: Int, t: T) extends In with TaskCoordinator.Out
  case class Result[R](i: Int, r: Try[R]) extends Out with TaskCoordinator.In
}

class TaskManager extends Actor {
  val log = Logging(context.system, this)
  var finished = Map[String, TaskCoordinator.Status]()

  def receive = {
    case x: TaskManager.In => x match {
      case TaskManager.GetTasks =>
      	sender() ! TaskManager.TaskNames(context.children.map(_.path.name))
      case TaskManager.GetCoordinator(name) =>
        val coordinatorProxy = if (finished.contains(name)) {
          context.actorOf(Props(classOf[FakeCoordinator], finished(name)))
        } else {
          context.actorOf(Props(classOf[CoordinatorProxy], context.child(name)))
        }
        sender() ! TaskManager.Coordinator(coordinatorProxy)
      case TaskManager.StartTask(analyzer, work, params) =>
        if (work.ts.size == 0) sender() ! TaskManager.CouldNotStart
        val uuid = java.util.UUID.randomUUID.toString
        val coordinator = context.actorOf(analyzer.actorProps(self, params), name = uuid)
        coordinator ! work
        sender() ! TaskManager.Started(uuid)
      case TaskManager.CoordinatorFinished(name, status) =>
        finished += name -> status
        sender() ! akka.actor.PoisonPill
    }
    case Terminated(child) =>
      println(s"${child.path.name} stopped")
//      finished += child.path.name -> TaskCoordinator.Failed
  }
}

class CoordinatorProxy(var coordinator: Option[ActorRef]) extends Actor {
  var lastSender = context.system.deadLetters

  if (coordinator.isDefined) {
    context.watch(coordinator.get)
  }

  def receive = {
    case in: TaskCoordinator.In =>
      lastSender = sender()
      coordinator.map { coord => coord ! in }.getOrElse(lastSender ! TaskCoordinator.NotFound)
    case o: TaskCoordinator.Out =>
      lastSender ! o
      coordinator.map(context.unwatch(_))
      context.stop(self)
    case Terminated(_) =>
      println("child has stopped")
      coordinator = None
  }
}

class FakeCoordinator(status: TaskCoordinator.Status) extends Actor {
  val log = Logging(context.system, this)

  def receive = {
    case x: TaskCoordinator.In => sender ! status
    case x => log.error(s"Unrecognized message: ${x.getClass}")
  }
}
package actors

import akka.actor._
import akka.routing._
import akka.event.Logging
import akka.pattern.pipe
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import models._
import scala.util.{ Try, Success, Failure }

object TaskManager {
  type TaskParams = Map[String, String]
  case object GetTasks
  case class TaskNames(xs: Iterable[String])

  case class StartTask[T](workerType: String, batch: TaskCoordinator.WorkBatch[T], params: TaskParams = Map())
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
    case TaskManager.StartTask(workerType: String, work: TaskCoordinator.WorkBatch[_], params: TaskManager.TaskParams) =>
      outside = Some(sender)
      if (work.ts.size == 0) forwardToController(TaskManager.CouldNotStart)
      val analyzer = Analyzers(workerType)
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

class TaskCoordinator[T: ClassTag, R: ClassTag, W <: TaskWorker[T, R]](
  replyTo: ActorRef,
  f: T => R,
  maxWorkers: Int = 1)(implicit workerTag: ClassTag[W]) extends Actor {
  var done = 0
  var total = -1
  var results = Array[Try[R]]()
  val log = Logging(context.system, this)
  val workerProps = Props(workerTag.runtimeClass, f)

  var router = {
    val routees = Vector.fill(maxWorkers) {
      val r = context.actorOf(workerProps)
      context watch r
      ActorRefRoutee(r)
    }
    Router(RoundRobinRoutingLogic(), routees)
  }

  def receive = {
    case TaskCoordinator.WorkBatch(ts: Seq[T]) =>
      total = ts.length
      val me = self
      results = Array.ofDim[Try[R]](total)
      for ((t, i) <- ts.view.zipWithIndex) {
        router.route(TaskWorker.WorkUnit(i, t), me)
      }
      sender ! TaskManager.Started(self.path.name)
    case Terminated(a) =>
      router = router.removeRoutee(a)
      val r = context.actorOf(workerProps)
      context watch r
      router = router.addRoutee(r)
    case TaskWorker.Result(i, res) =>
      results(i) = res.asInstanceOf[Try[R]]
      done += 1
      if (done == total) replyTo ! TaskCoordinator.Results(self.path.name, results)
    case TaskCoordinator.GetProgress =>
      if (total == -1) replyTo ! TaskCoordinator.NoWorkReceived
      else replyTo ! TaskCoordinator.Progress(self.path.toString, done.toDouble / total)
    case TaskCoordinator.Cancel =>
      // TODO what other resources need to be freed, if any?
      context.stop(self)
    case x =>
      log.error(s"TaskCoordinator didn't understand message: ${x.getClass}")
  }
}

class TaskWorker[T, R](f: T => R) extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case TaskWorker.WorkUnit(i, t) =>
      sender ! TaskWorker.Result(i, Try(f(t.asInstanceOf[T])))
    case x =>
      log.error(s"TaskWorker didn't understand message: ${x.getClass}")
  }
}
package actors

import akka.actor._
import akka.routing._
import akka.event.Logging
import scala.util.{ Try, Success, Failure }
import scala.reflect.ClassTag

import models.Text
import models.FullText

abstract class Analyzer[T: ClassTag, R: ClassTag] {
  type Params = TaskManager.TaskParams
  type F = T => R
  def makeF(params: Params): T => R

  class Coordinator(
    replyTo: ActorRef,
    workerClass: Class[_],
    f: F,
    maxWorkers: Int = 1) extends Actor {
    var done = 0
    var total = -1
    var results = Array[Try[R]]()
    val log = Logging(context.system, this)
    val workerProps = Props(workerClass, f)

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

  class Worker(f: F) extends Actor {
    val log = Logging(context.system, this)
    def receive = {
      case TaskWorker.WorkUnit(i, t) =>
        sender ! TaskWorker.Result(i, Try(f(t.asInstanceOf[T])))
      case x =>
        log.error(s"TaskWorker didn't understand message: ${x.getClass}")
    }
  }

  val coordinatorClass: Class[_]
  def actorProps(parent: ActorRef, params: Params) = {
    Props(coordinatorClass, parent, makeF(params))
  }
}

object TimesTwoAnalyzer extends Analyzer[Int, Int] {
  def makeF(p: Params) = {
    { x: Int => x * 2 }
  }
  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

object WordCountAnalyzer extends Analyzer[Text, Map[String, Int]] {
  def makeF(p: Params) = {
    { x: Text =>
      import FullText._
      val words = x.text.toLowerCase.split(" ")
      words.groupBy(identity).mapValues(_.length)
    }
  }
  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

object Analyzers {
  val analyzers = Map("timesTwo" -> TimesTwoAnalyzer, "word-count" -> WordCountAnalyzer)

  def apply(name: String) = analyzers.apply(name)
}
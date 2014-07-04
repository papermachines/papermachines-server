package actors

import akka.actor._
import akka.routing._
import akka.event.Logging
import scala.util.{ Try, Success, Failure }
import scala.reflect.ClassTag
import models.Text
import models.FullText
import play.api.data._
import play.api.data.Forms._
import java.net.URI
import java.io._
import org.joda.time.DateTime
import org.apache.tika.Tika

abstract class Analyzer[T, R] {
  type Params = TaskManager.Params
  type F = T => R

  val form: Option[Form[Params]] = None

  val name: String
  def makeF(params: Params): T => R
  val coordinatorClass: Class[_]

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

  def actorProps(parent: ActorRef, params: Params) = {
    Props(coordinatorClass, parent, makeF(params))
  }
}

object TimesTwoAnalyzer extends Analyzer[Int, Int] {
  val name = "times-two"
  def makeF(p: Params) = {
    { _ * 2 }
  }
  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

object WordCountAnalyzer extends Analyzer[Text, Map[String, Int]] {
  val name = "word-count"
  def makeF(p: Params) = {
    { x =>
      import FullText._
      val words = x.text.toLowerCase.split(" ")
      words.groupBy(identity).mapValues(_.length)
    }
  }
  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

object ExtractAnalyzer extends Analyzer[URI, Text] {
  val name = "extract"
  val tika = new Tika
    
  def copy(input: Reader, output: Writer, bufSize: Int = 2048) = {
    val buffer = Array.ofDim[Char](bufSize)
    var count = -1

    while ({ count = input.read(buffer); count > 0 })
      output.write(buffer, 0, count)
  }

  def makeF(p: Params) = {
    val outputDir = new File(URI.create(p("output-dir").toString))
    if (!outputDir.canWrite)
      throw new IllegalArgumentException(s"Cannot write to $outputDir!")
    
    { x =>
      val file = new File(x)
      val reader = tika.parse(file)
      val outputFile = new File(outputDir, file.getName + ".txt")
      val writer = new OutputStreamWriter(new FileOutputStream(outputFile), "UTF-8")
      
      copy(reader, writer)

      val newURI = outputFile.toURI
      Text(None, newURI, DateTime.now)
    }
  }
  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

object Analyzers {
  val analyzers = Seq(TimesTwoAnalyzer, WordCountAnalyzer)
  val analyzerMap = analyzers.map { x => x.name -> x }.toMap
  def apply(name: String) = analyzerMap.apply(name)
}
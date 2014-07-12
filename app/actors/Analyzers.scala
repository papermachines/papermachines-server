package actors

import akka.actor._
import akka.routing._
import akka.event.Logging
import scala.util.{ Try, Success, Failure }
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import java.net.URI
import java.io._
import org.joda.time.DateTime

import play.api.db.slick.Config.driver.simple._
import play.api.db.slick._

import models.Text
import models.FullText
import org.chrisjr.topic_annotator.corpora._

import shapeless._
import syntax.singleton._
import record._

abstract class Analyzer[T, R] {
  type Params = TaskManager.Params
  type F = T => R

  val form: Option[Form[Params]] = None

  val maxWorkers: Int = 1
  def makeF(params: Params): F
  val coordinatorClass: Class[_]

  def onDone(results: Seq[Try[R]]) = {}

  class Coordinator(
    replyTo: ActorRef,
    workerClass: Class[_],
    f: F) extends Actor {
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

    def getProgress = {
      done.toDouble / total
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
        val r = res.asInstanceOf[Try[R]]
        results(i) = r
        done += 1
        r match {
          case Success(x) =>
            log.debug(s"$x done.")
          case Failure(e) =>
            log.error(e.getMessage)
        }
        if (done == total) {
          onDone(results)
          replyTo ! TaskCoordinator.Results(self.path.name, results)
        }
      case TaskCoordinator.GetProgress =>
        if (total == -1) replyTo ! TaskCoordinator.NoWorkReceived
        else replyTo ! TaskCoordinator.Progress(self.path.toString, getProgress)
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
  def makeF(p: Params) = {
    { _ * 2 }
  }
  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

object WordCountAnalyzer extends Analyzer[Text, Map[String, Int]] {
  def makeF(p: Params) = {
    { x =>
      import FullText._
      val words = x.text.get.toLowerCase.split(" ")
      words.groupBy(identity).mapValues(_.length)
    }
  }
  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

object ExtractAnalyzer extends Analyzer[Text, Text] {
  import org.apache.tika.Tika
  import models.JsonImplicits._

  val tika = new Tika

  def copy(input: Reader, output: Writer, bufSize: Int = 2048) = {
    val buffer = Array.ofDim[Char](bufSize)
    var count = -1

    while ({ count = input.read(buffer); count > 0 })
      output.write(buffer, 0, count)
  }

  def makeF(p: Params) = {
    val outputDir = new File((p \ "output-dir").as[URI])
    if (!outputDir.canWrite)
      throw new IllegalArgumentException(s"Cannot write to $outputDir!")

    { x =>
      val file = new File(x.uri)
      val reader = tika.parse(file)
      val outputFile = new File(outputDir, file.getName + ".txt")
      val writer = new OutputStreamWriter(new FileOutputStream(outputFile), "UTF-8")

      copy(reader, writer)

      val newURI = outputFile.toURI
      x.copy(plaintextUri = Some(newURI))
    }
  }

  override def onDone(results: Seq[Try[Text]]) = {
    import play.api.db.slick._
    import play.api.Play.current

    DB.withSession { implicit s =>
      for (
        textTry <- results;
        text <- textTry if text.id.nonEmpty
      ) {
        models.Texts.update(text, text.plaintextUri.get)
      }
    }
  }

  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

trait DBAccess {
  def withDB[T](block: Session => T) = {
    import play.api.Play.current

    DB.withSession { implicit s =>
      block(s)
    }
  }
}

object Preprocessors {
  
}

trait TopicModelAnalyzer extends Analyzer[models.Corpus, org.chrisjr.topic_annotator.corpora.Corpus] with DBAccess {
  def makePreprocessingChain(p: Params) = {
    Seq(new MinLengthRemover(3))
  }
}

object HDPAnalyzer extends TopicModelAnalyzer {
  import models.CorpusImplicits._

  def makeF(p: Params) = {
    val transformers = makePreprocessingChain(p)

    { corpus =>
      withDB { implicit s =>
        val transformed = corpus.transform(transformers)
        transformed
      }
    }
  }

  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

object Analyzers {
  import ops.hlist.ToList
  import ops.record.{ Keys, Values }

  val corpusAnalyzers = ("hdp" ->> HDPAnalyzer) :: HNil

  def byName[B <: HList, K <: HList, V <: HList](name: String)(implicit keys: Keys.Aux[B, K],
    values: Values.Aux[B, V],
    ktl: ToList[K, Any],
    vtl: ToList[V, Any]) = {
    ((corpusAnalyzers.keys.toList zip corpusAnalyzers.values.toList) collect {
      case (field, value) if field == name => value
    }).headOption
  }

}
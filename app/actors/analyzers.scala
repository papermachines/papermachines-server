package actors

import akka.actor._
import akka.routing._
import akka.event.Logging
import scala.util.{ Try, Success, Failure }
import scala.collection.immutable
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import java.net.URI
import java.io._
import org.joda.time.DateTime
import models.DBAccess
import models.Text
import models.FullText
import org.chrisjr.topic_annotator.corpora._
import org.chrisjr.topic_annotator.topics._
import shapeless._
import syntax.singleton._
import record._
import models.Analysis

/**
 * Basic class to run NLP analyses.
 *
 * An Analyzer is built around a function of type F that transforms inputs to outputs.
 * It has a Coordinator to keep track of work and a set of Workers to perform it.
 *
 * Common Analyzers may be from Text to Text or from Corpus to Corpus.
 *
 * @tparam T input to function
 * @tparam R output (must be serializable)
 */
abstract class Analyzer[T, R <% Serializable] extends DBAccess {
  type Params
  type F = T => R

  /** Number of workers desired */
  val maxWorkers: Int = 1

  /**
   * Create the function to do the actual work.
   *
   * @param p the parameters to the analysis (if applicable, else None)
   */
  def makeF(p: Params): F
  val coordinatorClass: Class[_]

  var corpusID: Option[Long] = None
  var params: Option[TaskManager.Params] = None
  
  val analysisType: String
  
  /** By default, pickles the results inside the application-wide `resultsDir`.*/
  def onDone(requestID: String, results: Seq[Try[R]]): Long = {
    val resultDir = new File(Actors.resultsDir, requestID)
    resultDir.mkdirs()
    val resultFile = new File(resultDir, "result")
    Util.pickle(resultFile, results.toArray)

    val analysis = models.Analysis(
      corpusID = corpusID,
      analysisType = analysisType,
      params = params.getOrElse(Json.obj()),
      uri = resultFile.toURI,
      finishedAt = org.joda.time.DateTime.now()
    )

    withDB { implicit session =>
      models.Analyses.create(analysis)
    }
  }

  /** Maintain a pool of workers, reply to status requests, and indicate when finished. */
  class Coordinator(
    replyTo: ActorRef,
    workerClass: Class[_],
    f: F) extends Actor {
    var done = 0
    var total = -1
    var results = Array[Try[R]]()
    val resultDir = new File(Actors.resultsDir, self.path.name)
    val log = Logging(context.system, this)
    val workerProps = Props(workerClass, f)

    var router = {
      val routees = Vector.fill(maxWorkers) {
        val r = context.actorOf(workerProps)
        context.watch(r)
        ActorRefRoutee(r)
      }
      Router(RoundRobinRoutingLogic(), routees)
    }

    def getProgress = {
      done.toDouble / total
    }

    def receive = {
      case x: TaskCoordinator.In => x match {
        case TaskCoordinator.WorkBatch(ts: Seq[T]) =>
          resultDir.mkdirs()
          total = ts.length
          val me = self
          results = Array.ofDim[Try[R]](total)
          for ((t, i) <- ts.view.zipWithIndex) {
            router.route(TaskWorker.WorkUnit(i, t), me)
          }
        case TaskWorker.Result(i, res) =>
          val r = res.asInstanceOf[Try[R]]
          results(i) = r
          done += 1
          r match {
            case Success(x) =>              
              log.debug(s"$x done.")
            case Failure(e) =>
              log.error(Logging.stackTraceFor(e))
          }

          // TODO what if one of the workers never returns at all? should be a timeout?
          if (done == total) {
            val resultID = onDone(self.path.name, results)
            replyTo ! TaskManager.CoordinatorFinished(self.path.name, TaskCoordinator.Done(resultID))
          }
        case TaskCoordinator.GetProgress =>
          if (total == -1) sender() ! TaskCoordinator.NoWorkReceived
          else sender() ! TaskCoordinator.Progress(getProgress)
        case TaskCoordinator.Cancel =>
          // TODO what other resources need to be freed, if any?
          replyTo ! TaskManager.CoordinatorFinished(self.path.name, TaskCoordinator.Canceled)
      }
      case Terminated(a) =>
        router = router.removeRoutee(a)
        val r = context.actorOf(workerProps)
        context.watch(r)
        router = router.addRoutee(r)
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

  /** Turn a JSON object into appropriate Params object for this Analyzer. */
  def parseJson(p: TaskManager.Params): Params

  /** Get `Props` to build a new Coordinator actor. */
  def actorProps(parent: ActorRef, params: TaskManager.Params) = {
    Props(coordinatorClass, parent, makeF(parseJson(params)))
  }
}

object TimesTwoAnalyzer extends Analyzer[Int, Int] {
  val analysisType = "times-two"
  type Params = Unit
  def makeF(p: Params) = {
    { _ * 2 }
  }
  def parseJson(p: TaskManager.Params): Params = Unit
  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

object Preprocessors {
  def fromJson(j: JsObject): Option[CorpusTransformer] = {
    (j \ "name").asOpt[String].flatMap(_ match {
      case "min-length" => Some(new MinLengthRemover((j \ "length").asOpt[Int].getOrElse(3)))
      case _ => None
    })
  }
}

object WordCountAnalyzer extends Analyzer[Text, immutable.HashMap[String, Int]] {
  val analysisType="word-count"
  type Params = Unit
  def makeF(params: Params) = {
    { x =>
      import FullText._
      val words = x.text.get.toLowerCase.split(" ")
      immutable.HashMap[String, Int]() ++ words.groupBy(identity).mapValues(_.length)
    }
  }
  def parseJson(p: TaskManager.Params): Params = Unit
  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

object ExtractAnalyzer extends Analyzer[Text, Text] with DBAccess {
  import org.apache.tika.Tika
  import models.JsonImplicits._
  
  val analysisType = "extract"

  case class Params(outputDir: URI)
  override def parseJson(p: JsObject) = (p \ "output-dir").asOpt[URI].map(Params(_))
    .getOrElse(throw new IllegalArgumentException("No output dir specified."))

  val tika = new Tika

  def copy(input: Reader, output: Writer, bufSize: Int = 2048) = {
    val buffer = Array.ofDim[Char](bufSize)
    var count = -1

    while ({ count = input.read(buffer); count > 0 })
      output.write(buffer, 0, count)
  }

  def makeF(params: Params) = {
    val outputDir = new File(params.outputDir)
    if (!outputDir.canWrite)
      throw new IllegalArgumentException(s"Cannot write to $outputDir!")

    { x =>
      val file = new File(x.uri)
      val reader = tika.parse(file)
      val outputFile = new File(outputDir, x.outputFilename)
      val writer = new OutputStreamWriter(new FileOutputStream(outputFile), "UTF-8")

      copy(reader, writer)

      val newURI = outputFile.toURI
      x.copy(plaintextUri = Some(newURI))
    }
  }

  override def onDone(requestID: String, results: Seq[Try[Text]]) = {
    withDB { implicit s =>
      for (
        textTry <- results.filter(_.isSuccess);
        text <- textTry.toOption
        if text.id.nonEmpty && text.plaintextUri.nonEmpty
      ) {
        models.Texts.update(text, text.plaintextUri.get)
      }
    }

    super.onDone(requestID, results)
  }

  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

abstract class CorpusAnalyzer[R <% Serializable] extends Analyzer[models.Corpus, R]

object NoopAnalyzer extends CorpusAnalyzer[JsObject] {
  type Params = Unit
  val analysisType = "noop"
  def makeF(p: Params) = { _ => Json.obj() }

  def parseJson(p: TaskManager.Params): Params = {
    (p \ "corpusID").asOpt[Long].foreach { id => corpusID = Some(id) }
    Unit
  }
  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

trait TopicModelAnalyzer extends CorpusAnalyzer[JsObject] with DBAccess {
  import models.CorpusImplicits._
  import org.chrisjr.topic_annotator.utils.JsonUtils._

  case class Params(preprocessors: Seq[CorpusTransformer], tmParams: TopicModelParams)

  val modelType: TopicModel

  def makeF(params: Params) = {
    { corpus =>
      withDB { implicit s =>
        val transformed = corpus.transform(params.preprocessors)
        val annotated = modelType.annotate(transformed, params.tmParams)
        toPaperMachines(annotated, ???)
        Json.obj()
      }
      
    }
  }

  override def parseJson(p: JsObject) = {
    val preprocessors = ((p \ "prep").asOpt[Seq[JsObject]]).getOrElse(Seq()).flatMap(Preprocessors.fromJson)
    Params(preprocessors, TopicModelParams.defaultFor(modelType))
  }
}

object HDPAnalyzer extends TopicModelAnalyzer {
  val modelType = HDP
  val analysisType = "hdp"

  class WorkerImpl(f: F) extends Worker(f: F)
  class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
  val coordinatorClass = classOf[CoordinatorImpl]
}

/** Store all known analyzers (with their original types) and retrieve upon request. */
object Analyzers {
  import ops.hlist.ToList
  import ops.record.{ Keys, Values }

  val corpusAnalyzers = ("hdp" ->> HDPAnalyzer) :: HNil

  /** Retrieve the analyzer using its string. */
  def byName[B <: HList, K <: HList, V <: HList](name: String)(implicit keys: Keys.Aux[B, K],
    values: Values.Aux[B, V],
    ktl: ToList[K, Any],
    vtl: ToList[V, Any]) = {
    ((corpusAnalyzers.keys.toList zip corpusAnalyzers.values.toList) collect {
      case (field, value) if field == name => value
    }).headOption
  }

}
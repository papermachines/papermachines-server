package processors

import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import play.api.libs.json.JsObject
import scala.util.Try
import play.api.data.validation.ValidationError
import actors.CorpusAnalyzer
import models.{ DBAccess, Text, FullText }
import akka.actor.ActorRef

abstract class Processor extends DBAccess {
  import models.FullText._

  trait Request {
    def corpusID: Long
  }
  type ProcessRequest <: Request

  type Result = JsObject

  val name: String

  val form: Form[ProcessRequest]
  val analyzer: CorpusAnalyzer[Result]

  implicit val jsonFormat: Format[ProcessRequest]
  def writeJsonParams(p: ProcessRequest): JsObject = jsonFormat.writes(p).as[JsObject]

  val requestView = views.html.Analyses.start
  val resultView = views.html.index
}

object NoopProcessor extends Processor {
  val name = "noop"

  case class ProcessRequest(corpusID: Long, noop: Option[Int] = None) extends Request

  val form = Form(
    mapping(
      "corpusID" -> longNumber,
      "noop" -> optional(number))(ProcessRequest.apply)(ProcessRequest.unapply))

  val analyzer = actors.NoopAnalyzer

  implicit val jsonFormat = Json.format[ProcessRequest]
}

object WordCloudProcessor extends Processor {
  val name = "word-cloud"

  object ScaleType extends Enumeration {
    type ScaleType = Value
    val Raw, TfIdf, LogEnt = Value
  }

  import ScaleType._

  case class ProcessRequest(corpusID: Long, scaling: ScaleType) extends Request

  val form = Form(
    mapping(
      "corpusID" -> longNumber,
      "scaling" -> text.transform[ScaleType](ScaleType.withName, { x: ScaleType => x.toString }))(ProcessRequest.apply)(ProcessRequest.unapply))

  implicit object scaleTypeFormat extends Format[ScaleType] {
    def reads(js: JsValue): JsResult[ScaleType] = js match {
      case JsString(x) => Try(ScaleType.withName(x)).map(JsSuccess(_))
        .getOrElse(JsError(Seq(JsPath() -> Seq(ValidationError("error.expected.validscaletype")))))
      case _ => JsError(Seq(JsPath() -> Seq(ValidationError("error.expected.scaletype"))))
    }
    def writes(s: ScaleType): JsValue = JsString(s.toString)
  }

  implicit val jsonFormat: Format[ProcessRequest] = Json.format[ProcessRequest]

  object WordCloudAnalyzer extends actors.CorpusAnalyzer[Result] {
    import models.CorpusImplicits._
    import org.chrisjr.topic_annotator.corpora._

    type Params = ProcessRequest
    val analysisType: String = name
    
    val preprocessors = Seq(
        LowercaseTransformer
    )
    
    def makeF(p: Params) = {
      { corpus =>
        withDB { implicit s =>
          val tokenized = corpus.transform(preprocessors)
          val scored = new CorpusScorer(tokenized)
          val vocab = scored.vocabArray
          val tf = scored.tfOverall.map { case (k, v) => vocab(k) -> v}
          
          JsObject(tf.mapValues(JsNumber(_)).toSeq)
        }
      }
    }

    def parseJson(p: JsObject): Params = {
      params = Some(p)
      p.as[ProcessRequest]
    }

    class WorkerImpl(f: F) extends Worker(f: F)
    class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
    val coordinatorClass = classOf[CoordinatorImpl]
  }
  
  val analyzer = WordCloudAnalyzer
}

object Processors {
  val processors = Seq(
    WordCloudProcessor,
    NoopProcessor)
  val processorMap: Map[String, Processor] = processors.map(x => x.name -> x).toMap

  def byName(name: String): Option[Processor] = processorMap.get(name)
}
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

object Processors {
  val processors = Seq(
    WordCloudProcessor,
    NGramProcessor,
    MalletLdaProcessor,
    NoopProcessor)
  val processorMap: Map[String, Processor] = processors.map(x => x.name -> x).toMap

  def byName(name: String): Option[Processor] = processorMap.get(name)
}
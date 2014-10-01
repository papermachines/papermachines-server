package processors

import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import org.chrisjr.topic_annotator.corpora._
import org.chrisjr.topic_annotator.topics._
import actors.TopicModelAnalyzer
import akka.actor.ActorRef

object MalletLdaProcessor extends Processor {
  case class ProcessRequest(
    corpusID: Long,
    numTopics: Int,
    preprocessors: Seq[String] = Seq()
  ) extends Request

  val name = "mallet-lda"

  val form: Form[ProcessRequest] = Form(
    mapping(
      "corpusID" -> longNumber,
      "numTopics" -> number(min = 2),
      "preprocessors" -> seq(text)
    )(ProcessRequest.apply)(ProcessRequest.unapply))

  val analyzer = MalletLdaAnalyzer

  implicit val jsonFormat: Format[ProcessRequest] = Json.format[ProcessRequest]

  object MalletLdaAnalyzer extends TopicModelAnalyzer {
    val modelType = MalletLDA
    val analysisType = "mallet-lda"

    def outputDir = java.nio.file.Files.createTempDirectory("lda").toFile

    override def parseJson(p: JsObject) = {
      val preprocessors = preprocessorsFrom(p)
      val tmParams = TopicModelParams.defaultFor(modelType)
      tmParams.numTopics = (p \ "numTopics").as[Int]
      Params(preprocessors, tmParams)
    }

    class WorkerImpl(f: F) extends Worker(f: F)
    class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
    val coordinatorClass = classOf[CoordinatorImpl]
  }
}
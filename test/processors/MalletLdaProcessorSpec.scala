package processors

import org.scalatest._
import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import processors._
import controllers.Analyses
import actors.SpecWithActors
import scala.concurrent.Future
import play.api.mvc.Result
import scala.util.Try
import play.api.libs.json._

class MalletLdaProcessorSpec extends SpecWithActors with models.CorpusFixture with AnalysesCommon with TryValues {
  val processor = MalletLdaProcessor

  "The MalletLdaProcessor" should {
    "model topics in a corpus" in {
      val params = processor.ProcessRequest(corpusID, numTopics = 10)
      val result = startAnalysis(corpusID, processor)(params)
      
      status(result) mustEqual ACCEPTED
      
      val resultID = getResultID(result, wait = 500)
      
      val json = controllers.Tasks.getJson(resultID)
      val metadata = (json \ "DOC_METADATA").as[JsObject]
      
      metadata.fieldSet.size mustEqual sampleTexts.size
    }
  }
}
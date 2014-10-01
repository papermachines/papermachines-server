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

class WordCloudSpec extends SpecWithActors with models.CorpusFixture with AnalysesCommon with TryValues {
  val processor = WordCloudProcessor

  "The WordCloudProcessor" should {
    "generate raw term frequencies" in {
      val params = processor.ProcessRequest(corpusID, processor.ScaleType.Raw)
      val result = startAnalysis(corpusID, processor)(params)
      
      status(result) mustEqual ACCEPTED
      
      val resultID = getResultID(result)
      val results = controllers.Tasks.getResults[JsObject](resultID)
      results.head mustBe 'success

      val freqs = results.head.get
      freqs.fieldSet.size mustBe 26
      for ((word, freq) <- freqs.fieldSet) {
        freq.as[Int] mustEqual 100
      }
    }
  }
}
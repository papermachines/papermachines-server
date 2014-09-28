package processors

import org.scalatest._
import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import processors._
import controllers.Analyses
import scala.concurrent.Future
import play.api.mvc.Result
import scala.util.Try
import play.api.libs.json._

class WordCloudSpec extends PlaySpec with models.CorpusFixture with AnalysesCommon with TryValues {
  val processor = WordCloudProcessor

  "The WordCloudProcessor" should {
    "generate raw term frequencies" in {
      val params = processor.ProcessRequest(corpusID, processor.ScaleType.Raw)
      val result = startAnalysis(corpusID, processor)(params)
      
      status(result) mustEqual ACCEPTED
      val taskUrl = contentAsString(result)
      
      val resultUrl = retrieveResultUrl(taskUrl)
      
      val analysisIDr = "/analyses/([0-9]+)".r
      val resultID = resultUrl.getOrElse("") match {
        case analysisIDr(id) => id.toLong
        case _ => fail("No results found")
      }
      
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
package controllers

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import models.CorpusFixture
import processors._
import scala.concurrent.Future
import play.api.mvc.Result

class AnalysesSpec extends PlaySpec with CorpusFixture with AnalysesCommon {
  val processor = NoopProcessor

  "An Analysis" should {
    "be created on request" in {
      val procRequest = processor.ProcessRequest(corpusID, None)
      val analysisRequest = startAnalysis(corpusID, processor)(procRequest)
      status(analysisRequest) mustEqual ACCEPTED

      val resultUrl = retrieveResultUrl(contentAsString(analysisRequest))
      resultUrl.isDefined shouldBe true
    }

    "be retrievable from the DB" in {
      val procRequest = processor.ProcessRequest(corpusID, None)
      val analysisRequest = startAnalysis(corpusID, processor)(procRequest)
      status(analysisRequest) mustEqual ACCEPTED

      retrieveResultUrl(contentAsString(analysisRequest)).map({ resultUrl =>
        val result = route(FakeRequest(GET, resultUrl)).getOrElse(fail("Could not retrieve analysis."))
        status(result) mustEqual OK
      }).getOrElse(fail("No result found"))
    }
  }
}
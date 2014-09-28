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

class AnalysesSpec extends PlaySpec with CorpusFixture {
  val processor = NoopProcessor

  def startAnalysis(): Future[Result] = {
    val procRequest = processor.ProcessRequest(corpusID, None)
    val filledForm = processor.form.fill(procRequest)

    val request = FakeRequest(POST, s"/corpora/$corpusID/${processor.name}")
      .withFormUrlEncodedBody(filledForm.data.toSeq: _*)
    call(Analyses.create(processor.name), request)
  }

  val analysisIDr = "/analyses/([0-9]+)".r

  def retrieveID(taskUrl: String): Option[Long] = {
    route(FakeRequest(GET, taskUrl)).flatMap { result =>
      redirectLocation(result).map { resultUrl =>
        resultUrl match {
          case analysisIDr(id) => id.toLong
          case _ => fail("Could not get analysis ID.")
        }
      }
    }
  }

  "An Analysis" should {
    "be created on request" in {
      val analysisRequest = startAnalysis()
      status(analysisRequest) mustEqual ACCEPTED

      val analysisID = retrieveID(contentAsString(analysisRequest))
      analysisID.isDefined shouldBe true
    }

    "be retrievable from the DB" in {
      val analysisRequest = startAnalysis()
      status(analysisRequest) mustEqual ACCEPTED

      val analysisID = retrieveID(contentAsString(analysisRequest)).getOrElse(fail("Analysis ID could not be obtained."))
      val result = route(FakeRequest(GET, s"/analyses/$analysisID")).getOrElse(fail("Could not retrieve analysis."))
      status(result) mustEqual OK
    }
  }
}
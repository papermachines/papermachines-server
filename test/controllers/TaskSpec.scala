package controllers

import org.scalatest._
import org.scalatestplus.play._
import play.api.mvc._
import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._
import models.CorpusFixture

class TaskIntegrationSpec extends PlaySpec with CorpusFixture {  
  "Tasks" should {
    "be created from analysis requests" in {
      val request = FakeRequest(POST, "/corpora/1/analyses")
      val result = call(Analyses.create("noop"), request)

      status(result) mustEqual ACCEPTED
      val body = contentAsJson(result)
      (body \ "status") mustEqual "OK"
    }

    "be queryable for progress" in {

    }
  }
}
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
    }

    "be queryable for progress" in {
    }
  }
}
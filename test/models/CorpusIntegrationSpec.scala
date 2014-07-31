package models

import org.scalatest._
import org.scalatestplus.play._
import org.scalatest.Matchers._
import play.api.libs.json._

class CorpusIntegrationSpec extends PlaySpec with AppWithTestDB {
  val fakeTexts = (for {
    i <- 1 to 10
    uri = s"test$i"
  } yield Text(uri = new java.net.URI(uri)))

  "A Corpus" should {
    import Corpus._

    "be able to add new texts" in db { implicit s =>
      val newCorpusID = Corpora.fromTexts("test", fakeTexts)
      val corpusOpt = Corpora.find(newCorpusID)
      assert(corpusOpt.nonEmpty)
      assert(corpusOpt.get.texts.length == fakeTexts.length)
    }

    "be serializable to JSON" in {
      val corpus = Corpus(name = "Testing")
      val json = Json.toJson(corpus)
      json.toString.length should be > 0
    }

    "be deserializable from JSON" in {
      val input = """{"name":"Testing","externalID":"-1"}"""
      val corpusOpt = Json.parse(input).asOpt[Corpus]
      assert(corpusOpt.nonEmpty)
    }
  }
}
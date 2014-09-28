package models

import org.scalatest._
import org.scalatestplus.play._
import org.scalatest.Matchers._
import play.api.libs.json._
import org.joda.time.DateTime

class CorpusIntegrationSpec extends PlaySpec with AppWithTestDB {
  val fakeTexts = (for {
    i <- 1 to 10
    externalID = s"$i"
    uri = new java.net.URI(s"test$i")
  } yield Text(uri = uri, externalID = Some(externalID), lastModified = DateTime.parse("1999")))
  "A Corpus" should {
    import Corpus._

    "be creatable from texts" in db { implicit s =>
      val newCorpusID = Corpora.fromTexts("test", fakeTexts)
      val corpusOpt = Corpora.find(newCorpusID)
      assert(corpusOpt.nonEmpty)
      assert(corpusOpt.get.texts.length == fakeTexts.length)
    }

    "be able to update texts" in db { implicit s =>
      val newCorpusID = Corpora.fromTexts("test", fakeTexts)
      val updatedText = fakeTexts.head.copy(lastModified = DateTime.now())
      val status = Corpora.addTextTo(newCorpusID, updatedText)
      status shouldBe Texts.Updated
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
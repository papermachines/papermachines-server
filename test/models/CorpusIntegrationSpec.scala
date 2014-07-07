package models

import org.scalatest._
import org.scalatestplus.play._
import scala.slick.lifted.TableQuery
import play.api.db.slick._
import play.api.libs.json._
import scala.slick.jdbc.meta.MTable
import play.api.test.FakeApplication
import org.joda.time.DateTime

trait AppWithTestDB extends Suite with BeforeAndAfterAll with OneAppPerSuite {
  implicit override lazy val app: FakeApplication =
    FakeApplication(
      additionalConfiguration = Map("db.default.url" -> "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1"))

  override def beforeAll() = {
    DAL.createTablesIfNotExisting()
  }

  override def afterAll() = {
    DAL.dropTablesIfExisting()
  }

  def db(block: (Session => Unit)) = {
    DB.withSession { implicit s: Session =>
      block(s)
    }
  }
}

class CorpusIntegrationSpec extends PlaySpec with AppWithTestDB {
  val fakeTexts = (for {
    i <- 1 to 10
    uri = s"test$i"
  } yield Text(uri = new java.net.URI(uri)))

  "A Corpus" should {
    import CorpusJSON._

    "be able to add new texts" in db { implicit s =>
      val newCorpusID = Corpora.fromTexts("test", fakeTexts)
      val corpusOpt = Corpora.find(newCorpusID)
      assert(corpusOpt.nonEmpty)
      assert(corpusOpt.get.texts.length == fakeTexts.length)
    }

    "be serializable to JSON" in {
      val corpus = Corpus(name = "Testing")
      val json = Json.toJson(corpus)
      println(json)
    }

    "be deserializable from JSON" in {
      val input = """{"name":"Testing","externalID":"-1"}"""
      val corpusOpt = Json.parse(input).asOpt[Corpus]
      assert(corpusOpt.nonEmpty)
      println(corpusOpt.get)
    }
  }
}
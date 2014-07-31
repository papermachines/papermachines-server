package models

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatestplus.play._
import java.net.URI
import play.api.libs.json._
import org.joda.time.DateTime

class TextSpec extends PlaySpec with AppWithTestDB {
  val minimalText = Text(uri = new URI(""))
  val externalText = Text(
    uri = new URI(""),
    externalID = Some("-2"),
    lastModified = DateTime.parse("1999-01-01T05:00:00.000Z"))
  val textJson = """{"file-url": "", "last-modified": "1999-01-01T05:00:00.000Z"}"""

  "A Text" should {
    "be created from a case class" in db { implicit s =>
      val newID = Texts.create(minimalText)
      newID should be >= 0L
    }

    "be created from JSON" in db { implicit s =>
      val potentialText = Json.parse(textJson).validate[Text]
      potentialText.fold(
        errors => {
          fail(errors.toString)
        },
        text => {
          val textID = Texts.create(text)
          textID should be >= 0L
        })
    }

    "be added only once by external ID" in db { implicit s =>
      val (textID, initialStatus) = Texts.insertOrReplaceIfNewer(externalText)
      textID should be >= 0L
      initialStatus shouldEqual Texts.Created

      val (unchangedID, unchangedStatus) = Texts.insertOrReplaceIfNewer(externalText)
      unchangedID shouldEqual textID
      unchangedStatus shouldEqual Texts.Found
    }

    "be replaced if it has been modified recently" in db { implicit s =>
      val (textID, initialStatus) = Texts.insertOrReplaceIfNewer(externalText)

      val updatedText = externalText.copy(lastModified = DateTime.now())

      val (updatedID, updatedStatus) = Texts.insertOrReplaceIfNewer(updatedText)
      updatedStatus shouldEqual Texts.Updated
    }
  }

}
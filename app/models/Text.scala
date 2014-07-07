package models

import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.ForeignKeyQuery
import scala.slick.lifted.TableQuery
import play.api.db.slick._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.joda.time.DateTime
import scala.io._
import java.io._
import java.net.URI

case class Text(
  id: Option[Long] = None,
  uri: URI,
  metadata: JsObject = Json.obj(),
  lastModified: DateTime = DateTime.now,
  externalID: Option[String] = None,
  plaintextUri: Option[URI] = None) extends Item

object TextJSON {
  val uriReads: Reads[URI] = __.read[String].map(URI.create _)
  val uriWrites: Writes[URI] = (__.write[String]).contramap({ x: URI => x.toString })
  implicit val uriFormat: Format[URI] = Format(uriReads, uriWrites)
  
  implicit val cslPlusReads: Reads[Text] = (
    (__ \ "pm-id").readNullable[Long] and
    (__ \ "file-uri").read[URI] and
    (__.read[JsObject]) and
    (__ \ "last-modified").read[DateTime] and
    (__ \ "library-key").readNullable[String] and
    (__ \ "plaintext-uri").readNullable[URI]
  )(Text.apply _)
}

object FullText {
  implicit class EnrichedText(t: Text) {
    def text = t.plaintextUri map { uri =>
      val source = Source.fromFile(uri, "UTF-8")
      val docText = source.getLines.mkString("\n")
      source.close
      docText
    }
  }
}

class Texts(tag: Tag) extends TableWithAutoIncId[Text](tag, "TEXTS", "TEXT_ID") {
  def uri = column[URI]("TEXT_URI", O.NotNull)
  def metadata = column[JsObject]("TEXT_METADATA")
  def lastModified = column[DateTime]("TEXT_TIMESTAMP")
  def externalID = column[String]("TEXT_EXTID", O.Nullable)
  def plaintextUri = column[URI]("TEXT_PLAINURI", O.Nullable)

  def * = (id.?, uri, metadata, lastModified, externalID.?, plaintextUri.?) <> (Text.tupled, Text.unapply _)
}

object Texts extends BasicCrud[Texts, Text] {
  val table = TableQuery[Texts]
}
package models

import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.ForeignKeyQuery
import scala.slick.lifted.TableQuery
import play.api.db.slick._
import org.joda.time.DateTime
import scala.io._
import java.io._
import java.net.URI

case class Text(id: Option[Long] = None, uri: URI, lastModified: DateTime)

object FullText {
  implicit class EnrichedText(t: Text) {
    def text = {
      val source = Source.fromFile(t.uri, "UTF-8")
      val docText = source.getLines.mkString("\n")
      source.close
      docText
    }
  }
}

class Texts(tag: Tag) extends TableWithAutoIncId[Text](tag, "TEXTS", "TEXT_ID") {
  def uri = column[URI]("TEXT_URI", O.NotNull)
  def lastModified = column[DateTime]("TEXT_TIMESTAMP")

  def * = (id.?, uri, lastModified) <> (Text.tupled, Text.unapply _)
}

object Texts extends BasicCrud[Texts, Text] {
  val table = TableQuery[Texts]
}
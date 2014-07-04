package models

import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.ForeignKeyQuery
import scala.slick.lifted.TableQuery
import play.api.db.slick._
import scala.slick.jdbc.meta.MTable

case class Corpus(id: Option[Long] = None, name: String) {
  def texts(implicit s: Session): Seq[Text] = {
    val links = TableQuery[CorporaTexts]
    val texts = TableQuery[Texts]
    (for {
      link <- links.filter(_.corpusID === id.get)
      text <- texts if text.id === link.textID
    } yield text).list
  }
}

class Corpora(tag: Tag) extends TableWithAutoIncId[Corpus](tag, "CORPORA", "CORP_ID") {
  def name = column[String]("CORP_NAME")

  def * = (id.?, name) <> (Corpus.tupled, Corpus.unapply)
}

class CorporaTexts(tag: Tag) extends Table[(Long, Long)](tag, "CORPORA_TEXTS") {
  def corpusID = column[Long]("CORP_ID")
  def textID = column[Long]("TEXT_ID")

  def * = (corpusID, textID)

  def corpus = foreignKey("CORPTEXT_CORP_FK", corpusID, TableQuery[Corpora])(_.id)
  def text = foreignKey("CORPTEXT_TEXT_FK", textID, TableQuery[Texts])(_.id)
}

object Corpora extends BasicCrud[Corpora, Corpus] {
  val table = TableQuery[Corpora]
  val texts = TableQuery[Texts]
  val corporaTexts = TableQuery[CorporaTexts]

  /**
   * Add a corpus using a sequence of texts.
   *
   * @param name 	the name of the corpus
   * @param textsIn	the sequence of texts
   * @param s 		the DB session
   * @return 		the ID of the newly created corpus
   */
  def fromTexts(name: String, textsIn: Seq[Text])(implicit s: Session): Long = {
    val corpus = Corpus(None, name)
    val corpusID = (table returning table.map(_.id)) += corpus

    val newTextsAdded = addTextsTo(corpusID, textsIn)
    corpusID
  }
  
  def addTextsTo(corpusID: Long, textsIn: Seq[Text])(implicit s: Session): Int = {
    val (textsToAdd, textsAdded) = textsIn.partition(_.id.isEmpty)
    val oldIds = textsAdded.map(_.id.get)
    val newIds = (texts returning texts.map(_.id)) ++= textsToAdd
    corporaTexts ++= Stream.continually(corpusID) zip (oldIds ++ newIds)
    newIds.size
  }
}
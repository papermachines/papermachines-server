package models

import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.ForeignKeyQuery
import scala.slick.lifted.TableQuery
import play.api.db.slick._
import scala.slick.jdbc.meta.MTable
import play.api.libs.json._

import scala.util.{ Try, Success, Failure }
import org.chrisjr.topic_annotator.corpora._


case class Corpus(id: Option[Long] = None, name: String, externalID: Option[String] = None) extends Item {
  def texts(implicit s: Session): Seq[Text] = {
    val links = TableQuery[CorporaTexts]
    val texts = TableQuery[Texts]
    (for {
      link <- links.filter(_.corpusID === id.get)
      text <- texts if text.id === link.textID
    } yield text).list
  }
}

object Corpus {
  implicit val corpusFmt = Json.format[Corpus]  
}

object CorpusImplicits {
  import TextImplicits._

  implicit def corpusToTopicCorpus(corpus: Corpus)(implicit s: Session): org.chrisjr.topic_annotator.corpora.Corpus = {
    val texts = corpus.texts
    org.chrisjr.topic_annotator.corpora.Corpus(texts.map(textToTopicDocument))
  }
}

class Corpora(tag: Tag) extends TableWithAutoIncId[Corpus](tag, "CORPORA", "CORP_ID") {
  def name = column[String]("CORP_NAME")
  def externalID = column[String]("CORP_EXTID", O.Nullable)

  def * = (id.?, name, externalID.?) <> ((Corpus.apply _).tupled, Corpus.unapply)
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

  def addTextsTo(corpusID: Long, textsIn: Seq[Text])(implicit s: Session): (Int, Int) = {
    val textIds = for {
      text <- textsIn
      (id, isNew) = Texts.insertIfNotExistsByExternalID(text)      
    } yield (id, isNew)

    corporaTexts ++= Stream.continually(corpusID) zip (textIds.unzip._1)
    
    val (newIds, oldIds) = textIds.partition(_._2)
    (newIds.size, oldIds.size)
  }
  
  def insertIfNotExistsByExternalID(corpus: Corpus)(implicit s: Session) = {
    val existing = table.where(_.externalID === corpus.externalID).list
    existing.headOption match {
      case Some(corpusFound) =>
        (corpusFound.id.get, false)
      case None =>
        (create(corpus), true)
    }
  }
}
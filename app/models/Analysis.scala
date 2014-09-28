package models

import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.ForeignKeyQuery
import scala.slick.lifted.TableQuery
import play.api.db.slick._
import org.joda.time.DateTime
import java.net.URI
import actors.TaskManager
import play.api.libs.json._

case class Analysis(
  id: Option[Long] = None,
  corpusID: Option[Long] = None,
  analysisType: String,
  params: TaskManager.Params,
  uri: URI,
  finishedAt: DateTime) extends Item

object Analysis {
  import JsonImplicits._
  implicit val analysisFmt = Json.format[Analysis]
}

class Analyses(tag: Tag) extends TableWithAutoIncId[Analysis](tag, "ANALYSES", "ANALYSIS_ID") {
  def corpusID = column[Option[Long]]("CORP_ID")
  def analysisType = column[String]("ANALYSIS_TYPE")
  def params = column[JsObject]("ANALYSIS_PARAMS")
  def uri = column[URI]("ANALYSIS_URI")
  def finishedAt = column[DateTime]("ANALYSIS_TIMESTAMP")

  def * = (id.?, corpusID, analysisType, params, uri, finishedAt) <> ((Analysis.apply _).tupled, Analysis.unapply _)

  def corpus = foreignKey("ANALYSES_CORP_FK", corpusID, TableQuery[Corpora])(_.id.?)
}

object Analyses extends BasicCrud[Analyses, Analysis] {
  val table = TableQuery[Analyses]
}

trait AnalysisRequest {
  def corpusID: Long
}
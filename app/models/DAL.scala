package models

import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.ForeignKeyQuery
import scala.slick.lifted.TableQuery
import play.api.db.slick._
import scala.slick.jdbc.meta.MTable
import org.joda.time.DateTime
import java.sql.Timestamp
import scala.slick.lifted.CanBeQueryCondition
import java.net.URI

abstract class TableWithAutoIncId[T](tag: Tag, name: String, idName: String) extends Table[T](tag, name) {
  def id = column[Long](idName, O.PrimaryKey, O.AutoInc)

  implicit val dateTime = MappedColumnType.base[DateTime, Timestamp](
    dt => new Timestamp(dt.getMillis),
    ts => new DateTime(ts.getTime))

  implicit val uriMapping = MappedColumnType.base[URI, String](_.toString, URI.create(_))
}

trait BasicCrud[T <: TableWithAutoIncId[R], R] {
  val table: TableQuery[T]

  def create(item: R)(implicit s: Session): Long = {
    val itemID = (table returning table.map(_.id)) += item
    itemID
  }

  def find(id: Long)(implicit s: Session): Option[R] = table.where(_.id === id).firstOption

  def filter(pred: T => Column[Boolean])(implicit s: Session): Seq[R] =
    table.where(pred).list

  def count(implicit s: Session): Int = Query(table.length).first

  def delete(id: Long)(implicit s: Session): Boolean = {
    val rowsDeleted = table.where(_.id === id).delete
    rowsDeleted == 0
  }

  def list(implicit s: Session) = table.list
}

// TODO this isn't really a data access layer, but a set of utility functions for managing the DB
object DAL {
  import play.api.Play.current

  val analyses = TableQuery[Analyses]
  val corpora = TableQuery[Corpora]
  val texts = TableQuery[Texts]
  val corporaTexts = TableQuery[CorporaTexts]

  val tables = Map(
    "CORPORA" -> corpora,
    "TEXTS" -> texts,
    "CORPORA_TEXTS" -> corporaTexts,
    "ANALYSES" -> analyses)

  val ddls = tables.values.map(_.ddl).reduce(_ ++ _)

  def tableExists(tableName: String)(implicit s: Session) = {
    MTable.getTables(tableName).list.nonEmpty
  }
  def createTables(implicit s: Session) = ddls.create

  def dropTables(implicit s: Session) = ddls.drop

  def createTablesIfNotExisting() = {
    DB.withSession { implicit s =>
      val nonExistent = tables.collect { case (k, v) if !tableExists(k) => v }
      if (nonExistent.nonEmpty) {
        val ddls = nonExistent.map(_.ddl).reduce(_ ++ _)
        ddls.create
      }
    }
  }

  def dropTablesIfExisting() = {
    DB.withSession { implicit s =>
      val existent = tables.collect { case (k, v) if tableExists(k) => v }
      if (existent.nonEmpty) {
        val ddls = existent.map(_.ddl).reduce(_ ++ _)
        ddls.drop
      }
    }
  }
}
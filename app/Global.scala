import play.api._
import models._
import play.api.db.slick._
import play.api.Play.current
import scala.slick.jdbc.meta.MTable

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    Fixtures.insert()
  }

}

object Fixtures {
  def insert() = {
    DAL.createTablesIfNotExisting()

    //    DB.withSession { implicit s =>
    //    }
  }
}
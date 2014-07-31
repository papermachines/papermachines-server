package models

import org.scalatest._
import org.scalatestplus.play._
import play.api.db.slick._

import play.api.test.FakeApplication

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
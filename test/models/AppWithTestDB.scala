package models

import org.scalatest._
import org.scalatestplus.play._
import play.api.db.slick._
import play.api.test.FakeApplication
import org.joda.time.DateTime
import java.nio.file.Files

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

trait CorpusFixture extends AppWithTestDB {
  var corpusID = -1L
  val tempDir = Files.createTempDirectory(null).toFile()
  lazy val sampleTexts = {
    def writeText(file: java.io.File) = {
      val writer = new java.io.PrintWriter(file)
      (1 to 10).foreach { _ => writer.println((65 to 90).map(_.toChar).mkString(" ")) }
      writer.close
    }
    
    for {
      i <- 1 to 10
      externalID = s"$i"
      uri = new java.net.URI(s"test$i")
      textFile = new java.io.File(tempDir, s"test$i.txt")
      _ = writeText(textFile)
    } yield Text(
      uri = uri,
      plaintextUri = Some(textFile.toURI),
      externalID = Some(externalID),
      lastModified = DateTime.parse("1999"))
  }

  override def beforeAll() = {
    super.beforeAll()

    db { implicit s =>
      corpusID = Corpora.fromTexts("Test", sampleTexts)
    }
  }
  
  override def afterAll() = {
    super.afterAll()
    
    tempDir.deleteOnExit()
  }
}
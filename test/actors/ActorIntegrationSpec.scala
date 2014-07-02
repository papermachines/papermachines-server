package actors

import org.scalatest._
import org.scalatestplus.play._
import akka.actor._
import akka.actor.ActorDSL._
import scala.concurrent.duration._
import scala.language.postfixOps
import play.api.libs.concurrent.Akka
import java.nio.file.Files
import java.io._
import scala.util.Random
import models.Text
import org.joda.time.DateTime

import scala.util.control.Breaks._

class ActorIntegrationSpec extends PlaySpec with OneAppPerSuite {
  "A TimesTwoCoordinator" should {
    "multiply 10 inputs by two" in {
      val taskManager = Actors.taskManager
      implicit val system = Akka.system
      implicit val i = inbox()

      val input = Range(0, 10)
      taskManager ! TaskManager.StartTask("timesTwo", TaskCoordinator.WorkBatch(input))

      var finished = false
      while (!finished) {
        i.receive(100 milliseconds) match {
          case TaskManager.Started(name) =>
          // do nothing
          case TaskCoordinator.Results(_, maybeInts) =>
            maybeInts.filter(_.isSuccess).size mustBe input.size
            maybeInts.map(_.get) mustEqual input.map(_ * 2)
            finished = true
        }
      }
    }
    "multiply 10000 inputs by two" in {
      val taskManager = Actors.taskManager
      implicit val system = Akka.system
      implicit val i = inbox()

      val input = Range(0, 10000)
      taskManager ! TaskManager.StartTask("timesTwo", TaskCoordinator.WorkBatch(input))

      var finished = false
      while (!finished) {
        i.receive(10 seconds) match {
          case TaskManager.Started(name) =>
          // do nothing
          case TaskCoordinator.Results(_, maybeInts) =>
            maybeInts.filter(_.isSuccess).size mustBe input.size
            maybeInts.map(_.get) mustEqual input.map(_ * 2)
            finished = true
        }
      }
    }
  }

  def createFakeTexts = {
    val textDirPath = Files.createTempDirectory("texts")
    val textDir = textDirPath.toFile
    val vocab = (0 to 100).map(_.toString)
    def genDoc(length: Int) = {
      (0 until length).map({ _ => vocab(Random.nextInt(vocab.length)) }).mkString(" ")
    }
    for {
      i <- 0 to 1000
      newFile = File.createTempFile("text", ".txt", textDir)
      pw = new PrintWriter(newFile, "UTF-8")
      _ = pw.print(genDoc(i))
      _ = pw.close
      uri = newFile.toURI()
    } yield Text(uri = uri, lastModified = new DateTime)
  }

  "A WordCountCoordinator" should {
    "count words in a set of texts" in {
      val texts = createFakeTexts
      val taskManager = Actors.taskManager
      implicit val system = Akka.system
      implicit val i = inbox()
      taskManager ! TaskManager.StartTask("word-count", TaskCoordinator.WorkBatch(texts))

      val myName = i.receive(1 second) match {
        case TaskManager.Started(name) =>
          name
      }

      breakable {
        for (_ <- 0 to 100) {
          taskManager ! TaskManager.GetProgressFor(myName)
          i.receive(1 second) match {
            case TaskCoordinator.Progress(name, amt) =>
              println(f"${amt * 100.0}%2.2f%%")
              Thread.sleep(100)
            case TaskCoordinator.Results(_, intmaps) =>
              intmaps.size mustBe texts.size
              break
          }
        }
      }
    }
  }
}
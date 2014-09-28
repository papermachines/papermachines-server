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
import scala.io._
import scala.util.Random
import models.Text
import org.joda.time.DateTime
import scala.util.{ Try, Success, Failure }
import scala.util.control.Breaks._
import play.api.libs.json._

class ActorIntegrationSpec extends PlaySpec with OneAppPerSuite {

  def testAnalyzer[T, R](
    analyzer: Analyzer[T, R],
    input: Seq[T],
    onSuccess: Seq[Try[R]] => Unit,
    params: TaskManager.Params = Json.obj(),
    checkInterval: Int = 100)(implicit app: play.api.Application) = {
    val taskManager = Actors.taskManager
    implicit val system = Akka.system
    implicit val i = inbox()

    taskManager ! TaskManager.StartTask(analyzer, TaskCoordinator.WorkBatch(input), params)

    val myName = i.receive(1 second) match {
      case TaskManager.Started(name) =>
        name
    }

    checkProgress(taskManager, myName, onSuccess)
  }

  def checkProgress[R](taskManager: ActorRef, myName: String, onSuccess: Seq[Try[R]] => Unit)(implicit i: ActorDSL.Inbox) = {
    breakable {
      for (_ <- 0 to 100) {
        val coordinator = controllers.Tasks.getCoordinator(myName)
        coordinator ! TaskCoordinator.GetProgress
        i.receive(1 second) match {
          case x: TaskCoordinator.Out => x match {
            case TaskCoordinator.Progress(amt) =>
              println(f"${amt * 100.0}%2.2f%%")
              Thread.sleep(100)
            case TaskCoordinator.Done(resultID) =>
              val results = controllers.Tasks.getResults[R](resultID)
              onSuccess(results)
              break
            case TaskCoordinator.Canceled =>
            case TaskCoordinator.Failed =>
            case TaskCoordinator.NoWorkReceived =>
            case TaskCoordinator.NotFound =>
            case TaskCoordinator.Results(_, _) =>
            case TaskWorker.WorkUnit(_, _) =>
              break
          }
          case x =>
            println(s"Unknown message ${x.getClass} received")
            break
        }
      }
    }
  }

  def readInString(file: File) = {
    val source = Source.fromFile(file)
    val string = source.getLines.mkString("\n")
    source.close
    string
  }

  "A TimesTwoAnalyzer" should {
    "multiply 10 inputs by two" in {
      val input = Range(0, 10)
      testAnalyzer(TimesTwoAnalyzer, input, { maybeInts: Seq[Try[Int]] =>
        maybeInts.filter(_.isSuccess).size mustBe input.size
        maybeInts.map(_.get) mustEqual input.map(_ * 2)
      })
    }

    "multiply 10000 inputs by two" in {
      val input = Range(0, 10000)
      testAnalyzer(TimesTwoAnalyzer, input, { maybeInts: Seq[Try[Int]] =>
        maybeInts.filter(_.isSuccess).size mustBe input.size
        maybeInts.map(_.get) mustEqual input.map(_ * 2)
      })
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
    } yield Text(uri = uri, plaintextUri = Some(uri), metadata = Json.obj(), lastModified = new DateTime)
  }

  "A WordCountAnalyzer" should {
    "count words in a set of fake texts" in {
      val texts = createFakeTexts
      testAnalyzer(WordCountAnalyzer, texts, { intmaps: Seq[Try[Map[String, Int]]] =>
        intmaps.size mustBe texts.size
      })
    }
  }

  "An ExtractAnalyzer" should {
    "extract plain texts from PDFs" in {
      val leavesDir = new File(getClass.getResource("leaves").toURI)
      val pdfFilter = new java.io.FilenameFilter {
        def accept(d: File, n: String) = n.toLowerCase.endsWith(".pdf")
      }
      val pdfs = leavesDir.listFiles(pdfFilter).map(_.toURI).map { uri =>
        Text(uri = uri)
      }

      val outputPath = Files.createTempDirectory("output")
      val outputUri = outputPath.toUri
      val params = Json.obj("output-dir" -> JsString(outputUri.toString))

      testAnalyzer(ExtractAnalyzer, pdfs, { texts: Seq[Try[Text]] =>
        texts.size mustBe pdfs.size
        for (textTry <- texts) {
          textTry.isSuccess mustBe true
          val text = textTry.get.asInstanceOf[Text]
          text.plaintextUri.nonEmpty mustBe true
          text.plaintextUri.map { uri =>
            val result = new File(uri)
            result.exists mustBe true

            val string = readInString(result)
            string.size must be > 0
          }
        }
      }, params)
    }
  }
}
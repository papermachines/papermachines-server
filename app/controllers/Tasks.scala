package controllers

import actors._
import akka.actor._
import akka.actor.ActorDSL._
import akka.util.Timeout
import play.api._
import play.api.libs.concurrent.Akka
import play.api.Play.current
import play.api.db.slick._
import play.api.db.slick.Config.driver.simple._
import play.api.mvc._
import play.api.libs.json._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{ Try, Success, Failure }

object Tasks extends Controller {
  val taskManager = Actors.taskManager
  implicit val system = Akka.system
  implicit val i = inbox()
  implicit val timeout = 10 seconds
  
  def getCoordinator(name: String): ActorRef = {
    taskManager ! TaskManager.GetCoordinator(name)
    i.receive(timeout) match {
      case TaskManager.Coordinator(ref) => ref
      case x => throw new IllegalStateException(s"Unknown message $x received.")
    }
  }

  def index = Action {
    taskManager ! TaskManager.GetTasks
    val names = i.receive(timeout) match {
      case TaskManager.TaskNames(xs) =>
        xs
    }
    Ok(names.mkString("\n"))
  }

  def find(id: String) = Action {
    val ref = getCoordinator(id)
    ref ! TaskCoordinator.GetProgress
    i.receive(timeout) match {
      case TaskCoordinator.Done(resultID) =>
        Redirect(routes.Analyses.find(resultID))
      case TaskCoordinator.Progress(amt) =>
        Ok(views.html.Tasks.progress((amt * 100.0).toInt))
      case TaskCoordinator.NotFound =>
        NotFound
    }
  }

  def startTask[T, R](analyzer: Analyzer[T, R], work: Seq[T], params: TaskManager.Params = Json.obj()): Try[String] = {
    taskManager ! TaskManager.StartTask(analyzer, TaskCoordinator.WorkBatch(work), params)
    i.receive(timeout) match {
      case TaskManager.Started(name) =>
        Success(name)
      case TaskManager.CouldNotStart =>
        Failure(new IllegalStateException("Could not start"))
    }
  }

  def create = Action {
    val input = Seq[Int](1, 2)
    val analyzer = TimesTwoAnalyzer
    startTask(analyzer, input) match {
      case Success(name) =>
        Accepted(<a href="@routes.Tasks.find(name)">Status</a>)
      case Failure(e) =>
        Ok("could not start")
    }
  }
  
  def getResults[R](resultID: Long) = {
    import org.chrisjr.topic_annotator.corpora.Util
    DB.withSession { implicit s =>
      val analysis = models.Analyses.find(resultID)
        .getOrElse(throw new IllegalArgumentException(s"No analysis $resultID found"))
      Util.unpickle[Array[Try[R]]](new java.io.File(analysis.uri))    
    }
  }

  /**
   * Cancels a task in progress (if already completed, does nothing).
   *
   * @param id
   * @return
   */
  def delete(id: String) = Action {
    val ref = getCoordinator(id)
    ref ! TaskCoordinator.Cancel
    i.receive(timeout) match {
      case TaskCoordinator.Canceled =>
        Ok("Deleted")
      case TaskCoordinator.NotFound =>
        NotFound
    }
  }
}
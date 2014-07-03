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
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{ Try, Success, Failure }

object Tasks extends Controller {
  val taskManager = Actors.taskManager
  implicit val system = Akka.system
  implicit val i = inbox()
  implicit val timeout = 10 seconds

  def index = Action {
    taskManager ! TaskManager.GetTasks
    val names = i.receive(timeout) match {
      case TaskManager.TaskNames(xs) =>
        xs
    }
    Ok(names.mkString("\n"))
  }

  def find(id: String) = Action {
    taskManager ! TaskManager.GetProgressFor(id)
    i.receive(timeout) match {
      case TaskManager.Done(resultID) =>
        Redirect(routes.Analyses.find(resultID))
      case TaskCoordinator.Progress(name, amt) =>
        Ok(amt.toString)
      case TaskManager.TaskNotFound =>
        NotFound
    }
  }

  def startTask[T, R](analyzer: Analyzer[T, R], work: Seq[T]) = {
    taskManager ! TaskManager.StartTask(analyzer, TaskCoordinator.WorkBatch(work))
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

  /**
   * Cancels a task in progress (if already completed, does nothing).
   *
   * @param id
   * @return
   */
  def delete(id: String) = Action {
    taskManager ! TaskManager.CancelTask(id)
    i.receive(timeout) match {
      case TaskManager.Canceled(name) if name == id =>
        Ok("Deleted")
      case TaskManager.TaskNotFound =>
        NotFound
    }
  }
}
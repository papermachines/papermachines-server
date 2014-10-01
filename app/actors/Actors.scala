package actors

import play.api._
import play.api.libs.concurrent.Akka
import java.net.URL
import akka.actor._
import akka.pattern.ask
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Await


/** Lookup for actors used by the web front end.
  */
object Actors {

  private def actors(implicit app: Application) = app.plugin[Actors]
    .getOrElse(sys.error("Actors plugin not registered"))

  def taskManager(implicit app: Application) = actors.taskManager
  
  val resultsDir = new java.io.File("/tmp")
  
  def sendAndAwait[T, R](actor: ActorRef, msg: T, timeout: akka.util.Timeout, f: Function1[Any, R])(implicit app: Application): R =
    actors.sendAndAwait(actor, msg, timeout, f)
}

/**
  * Manages the creation of actors in the web front end.
  *
  * This is discovered by Play in the `play.plugins` file.
  */
class Actors(app: Application) extends Plugin {

  private def system = Akka.system(app)
  
  var taskManager: ActorRef = null

  override def onStart() = {
    taskManager = system.actorOf(Props[TaskManager], name = "taskManager")
  }
  
  def sendAndAwait[T, R](actor: ActorRef, msg: T, timeout: akka.util.Timeout, f: Function1[Any, R]): R = {
    Await.result(actor.ask(msg)(timeout).map(f), timeout.duration + (1 second))
  }

}
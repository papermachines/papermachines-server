package actors

import play.api._
import play.api.libs.concurrent.Akka
import java.net.URL

import akka.actor._

/**
 * Lookup for actors used by the web front end.
 */
object Actors {

  private def actors(implicit app: Application) = app.plugin[Actors]
    .getOrElse(sys.error("Actors plugin not registered"))

  def taskManager(implicit app: Application) = actors.taskManager
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
}
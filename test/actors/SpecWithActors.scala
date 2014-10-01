package actors

import org.scalatest._
import org.scalatestplus.play._
import akka.actor._
import akka.actor.ActorDSL._
import akka.testkit._
import scala.concurrent.duration._
import play.api.Application
import play.api.libs.concurrent.Akka
import play.api.test.FakeApplication

class SpecWithActors extends PlaySpec with TestKitBase with ImplicitSender with models.AppWithTestDB {
  implicit lazy val system: ActorSystem = Akka.system(app)
  lazy val taskManager = Actors.taskManager(app)
  
  def getCoordinator(name: String) = {
    taskManager ! TaskManager.GetCoordinator(name)
    expectMsgPF(5 seconds) {
      case TaskManager.Coordinator(ref) => ref
      case x => throw new IllegalStateException(s"Unknown message $x received.")
    }

  }
}
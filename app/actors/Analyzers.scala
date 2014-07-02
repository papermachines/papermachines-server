package actors

import akka.actor._
import scala.reflect.ClassTag
import models.Text
import models.FullText

abstract class Analyzer[T: ClassTag, R: ClassTag] {
  type Params = TaskManager.TaskParams
  type F = T => R
  def makeF(params: Params): T => R

  val coordinatorClass: Class[_]
  def actorProps(parent: ActorRef, params: Params) = {
    Props(coordinatorClass, parent, makeF(params))
  }
}

object TimesTwoAnalyzer extends Analyzer[Int, Int] {
  class Worker(f: F) extends TaskWorker(f)
  class Coordinator(replyTo: ActorRef, f: F) extends TaskCoordinator[Int, Int, Worker](replyTo, f)
  val coordinatorClass = classOf[Coordinator]

  def makeF(p: Params) = {
    { x: Int => x * 2 }
  }
}

object WordCountAnalyzer extends Analyzer[Text, Map[String, Int]] {
  class Worker(f: F) extends TaskWorker(f)
  class Coordinator(replyTo: ActorRef, f: F) extends TaskCoordinator[Text, Map[String, Int], Worker](replyTo, f)
  val coordinatorClass = classOf[Coordinator]

  def makeF(p: Params) = {
    { x: Text =>
      import FullText._
      val words = x.text.toLowerCase.split(" ")
      words.groupBy(identity).mapValues(_.length)
    }
  }
}

object Analyzers {
  val analyzers = Map("timesTwo" -> TimesTwoAnalyzer, "word-count" -> WordCountAnalyzer)

  def apply(name: String) = analyzers.apply(name)
}
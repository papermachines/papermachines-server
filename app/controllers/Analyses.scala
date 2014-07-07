package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import play.api.Play.current

object Analyses extends Controller {
  def index = Action {
    DB.withSession { implicit s =>
      val analyses = models.Analyses.list
      Ok(views.html.Analyses.index(analyses))
    }
  }

  def create = Action {
    DB.withSession { implicit s =>
      val status = "Beginning to process"
      Accepted(status)
    }
  }

  def find(id: Long) = Action {
    DB.withSession { implicit s =>
      val analysisOpt = models.Analyses.find(id)
      analysisOpt match {
        case Some(analysis) =>
          // TODO figure out where analyses should really reside
          Redirect(analysis.uri.toURL.toString)
        case None => NotFound
      }
    }
  }

  def delete(id: Long) = Action {
    DB.withSession { implicit s =>
      models.Analyses.delete(id)
      Ok("Deleted")
    }
  }
}
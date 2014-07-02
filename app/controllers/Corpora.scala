package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import play.api.Play.current

object Corpora extends Controller {
  def index = Action {
    DB.withSession { implicit s =>
      val corpora = models.Corpora.list
      Ok(corpora.mkString("\n"))
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
      val corpusOption = models.Corpora.find(id)
      corpusOption match {
        case Some(corpus) =>
          val rendered = corpus.name
          Ok(rendered)
        case None => NotFound
      }
    }
  }

  def delete(id: Long) = Action {
    DB.withSession { implicit s =>
      models.Corpora.delete(id)
      Ok("Deleted")
    }
  }
}
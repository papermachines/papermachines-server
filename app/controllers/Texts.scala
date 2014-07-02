package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import play.api.Play.current

object Texts extends Controller {
  def index = Action {
    Ok("list of texts")
  }

  def create = Action {
    Accepted("Adding a text")
  }

  def find(id: Long) = Action {
    Ok("a text")
  }

  def delete(id: Long) = Action {
    DB.withSession { implicit s =>
      val textOption = models.Texts.find(id)
      Ok("Removed")
    }
  }
}
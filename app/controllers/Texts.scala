package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import play.api.Play.current
import play.api.libs.json._

import models.TextJSON._

object Texts extends Controller {
  def index = Action { implicit request =>
    DB.withSession { implicit s =>
      val texts = models.Texts.list
      render {
        case Accepts.Html() => Ok(views.html.Texts.index(texts))
        case Accepts.Json() => Ok(Json.toJson(texts))
      }
    }
  }

  def create = Action(parse.json) { implicit request =>
    val textResult = request.body.validate[models.Text]
    textResult.fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toFlatJson(errors)))
      },
      text => {
        DB.withSession { implicit s =>
          val (id, isNew) = models.Texts.findOrCreateByExternalID(text)
          val reply = Json.obj("status" -> "OK", "id" -> id)
          if (isNew) Created(reply) else Ok(reply)
        }
      })
  }
  def find(id: Long) = Action {
    DB.withSession { implicit s =>
      val textOption = models.Texts.find(id)
      textOption match {
        case Some(text) =>
          Ok(views.html.Texts.text(text))
        case None => NotFound
      }
    }
  }

  def delete(id: Long) = Action {
    DB.withSession { implicit s =>
      models.Texts.delete(id)
      Ok("Deleted")
    }
  }
}
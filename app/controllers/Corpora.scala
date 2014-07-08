package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import play.api.Play.current

import play.api.libs.json._
import models.CorpusJSON._
import models.TextJSON._

object Corpora extends Controller {
  def index = Action { implicit request =>
    DB.withSession { implicit s =>
      val corpora = models.Corpora.list
      render {
        case Accepts.Html() => Ok(views.html.Corpora.index(corpora))
        case Accepts.Json() => Ok(Json.toJson(corpora))
      }
    }
  }

  def create = Action(parse.json) { request =>
    val corpusResult = request.body.validate[models.Corpus]
    corpusResult.fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toFlatJson(errors)))
      },
      corpus => {
        DB.withSession { implicit s =>
          val (id, isNew) = models.Corpora.insertIfNotExistsByExternalID(corpus)
          val reply = Json.obj("status" -> "OK", "id" -> id)
          if (isNew) Created(reply) else Ok(reply)
        }
      })
  }

  def find(id: Long) = Action {
    DB.withSession { implicit s =>
      val corpusOption = models.Corpora.find(id)
      corpusOption match {
        case Some(corpus) =>
          val texts = corpus.texts
          Ok(views.html.Corpora.corpus(corpus, texts))
        case None => NotFound
      }
    }
  }

  def getTexts(id: Long) = Action {
    DB.withSession { implicit s =>
      val corpusOption = models.Corpora.find(id)
      corpusOption match {
        case Some(corpus) =>
          val texts = corpus.texts
          Ok(views.html.Corpora.corpustexts(corpus, texts))
        case None => NotFound
      }
    }
  }
  
  def addTextTo(id: Long) = Action(parse.json) { request =>
    val textResult = request.body.validate[models.Text]
    textResult.fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toFlatJson(errors)))
      },
      text => {
        DB.withSession { implicit s =>
          val corpusOpt = models.Corpora.find(id)
          corpusOpt match {
            case Some(corpus) =>
              val (oldTexts, newTexts) = models.Corpora.addTextsTo(id, Seq(text))
              val reply = Json.obj("status" -> "OK", "id" -> id)
              if (newTexts > 0) Created(reply) else Ok(reply)
            case None =>
              val message = Json.obj("id" -> id, "error" -> "Corpus does not exist")
              BadRequest(Json.obj("status" -> "KO", "message" -> message))
          }
        }
      })
  }

  def delete(id: Long) = Action {
    DB.withSession { implicit s =>
      models.Corpora.delete(id)
      Ok("Deleted")
    }
  }
}
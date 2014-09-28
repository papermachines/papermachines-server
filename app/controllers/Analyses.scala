package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.db.slick._
import play.api.Play.current
import play.api.libs.json._
import models.Analysis
import processors._
import scala.util.{ Try, Success, Failure }
import play.core.Router
import actors.Actors

object Analyses extends Controller {
  import Analysis._
  import models.JsonImplicits._
  import models.CorpusImplicits._

  def index = Action {
    DB.withSession { implicit s =>
      val analyses = models.Analyses.list
      Ok(views.html.Analyses.index(analyses))
    }
  }

  def indexFor(corpusID: Long) = Action {
    DB.withSession { implicit s =>
      val analyses = models.Analyses.list.filter(_.corpusID == Some(corpusID))
      Ok(views.html.Analyses.index(analyses))
    }
  }

  def extract(id: Long) = Action {
    val analyzer = actors.ExtractAnalyzer
    DB.withSession { implicit session =>
      val corpusOpt = models.Corpora.find(id)
      corpusOpt match {
        case Some(corpus) =>
          val work = corpus.texts
          val outputDir = new java.io.File(actors.Actors.resultsDir, "texts").toURI
          val params = Json.obj("output-dir" -> Json.toJson(outputDir))
          controllers.Tasks.startTask(analyzer, work, params) match {
            case Success(name) =>
              val resultURL = routes.Tasks.find(name)
              Redirect(resultURL)
            //              Accepted(Json.obj("status" -> "OK", "message" -> resultURL.toString))
            case Failure(e) =>
              val exc = UnexpectedException(Some("Analysis failed"), Some(e))
              InternalServerError(views.html.defaultpages.error(exc))
          }
        case None =>
          BadRequest(Json.obj("status" -> "KO", "message" -> s"Corpus $id not found!"))
      }
    }
  }

  def create(processorName: String) = Action { implicit request =>
    Processors.byName(processorName).fold(BadRequest(s"No process $processorName found.")) { processor =>
      processor.form.bindFromRequest.fold(
        errors => {
          BadRequest(processor.requestView(processor, Some(errors)))
        },
        params => {
          DB.withSession { implicit session =>
            models.Corpora.find(params.corpusID)
              .fold(BadRequest(s"Corpus ${params.corpusID} not found!")) { corpus =>
                val work = Seq(corpus)
                controllers.Tasks.startTask(processor.analyzer, work, processor.writeJsonParams(params)) match {
                  case Success(name) =>
                    val resultURL = routes.Tasks.find(name)
                    Accepted(resultURL.toString)
                  case Failure(e) =>
                    val exc = UnexpectedException(Some("Analysis failed"), Some(e))
                    InternalServerError(views.html.defaultpages.error(exc))
                }
              }
          }
        })
    }
  }

  def find(id: Long) = Action { implicit request =>
    DB.withSession { implicit s =>
      val analysisOpt = models.Analyses.find(id)
      analysisOpt match {
        case Some(analysis) =>
          // TODO figure out where analyses should really reside
          Redirect(analysis.uri.toURL.toString)
        case None => NotFound(views.html.defaultpages.notFound(request, None))
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
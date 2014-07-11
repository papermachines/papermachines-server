package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import play.api.Play.current
import play.api.libs.json._
import models.Analysis
import actors.Analyzer
import actors.Analyzers
import scala.util.{ Try, Success, Failure }
import play.core.Router

import shapeless._
import record._

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

  def extract(id: Long) = Action {
    val analyzer = actors.ExtractAnalyzer
    DB.withSession { implicit session =>
      val corpusOpt = models.Corpora.find(id)
      corpusOpt match {
        case Some(corpus) =>
          val work = corpus.texts
          val outputDir = new java.io.File("/Users/chrisjr/Desktop/sstexts").toURI
          val params = Json.obj("output-dir" -> Json.toJson(outputDir))
          controllers.Tasks.startTask(analyzer, work, params) match {
            case Success(name) =>
              val resultURL = routes.Tasks.find(name)
              Accepted(Json.obj("status" -> "OK", "message" -> resultURL.toString))
            case Failure(e) =>
              val exc = UnexpectedException(Some("Analysis failed"), Some(e))
              InternalServerError(views.html.defaultpages.error(exc))
          }
        case None =>
          BadRequest(Json.obj("status" -> "KO", "message" -> s"Corpus $id not found!"))
      }
    }

  }

  def create = Action(parse.json) { implicit request =>
    val analysisRequest = request.body.validate[Analysis]
    analysisRequest.fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toFlatJson(errors)))
      },
      analysis => {
        Analyzers.byName(analysis.analysisType) match {
          case Some(analyzer) =>
            DB.withSession { implicit session =>
              val corpusOpt = models.Corpora.find(analysis.corpusID)
              corpusOpt match {
                case Some(corpus) =>
                  val work = Seq(corpus)
                  controllers.Tasks.startTask(analyzer, work) match {
                    case Success(name) =>
                      val resultURL = routes.Tasks.find(name)
                      Accepted(Json.obj("status" -> "OK", "message" -> resultURL.toString))
                    case Failure(e) =>
                      val exc = UnexpectedException(Some("Analysis failed"), Some(e))
                      InternalServerError(views.html.defaultpages.error(exc))
                  }
                case None =>
                  BadRequest(Json.obj("status" -> "KO", "message" -> s"Corpus ${analysis.corpusID} not found!"))
              }
            }
          case None =>
            BadRequest(Json.obj("status" -> "KO", "message" -> s"No analyzer of type ${analysis.analysisType}!"))
        }
      })
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
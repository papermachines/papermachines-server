package processors

import play.api.test._
import play.api.test.Helpers._
import scala.concurrent.Future
import play.api.mvc.Result

trait AnalysesCommon {
  def startAnalysis(
    corpusID: Long,
    processor: Processor)(procRequest: processor.ProcessRequest): Future[Result] = {
    require(corpusID > 0, "Invalid corpus ID")
    val filledForm = processor.form.fill(procRequest)

    val request = FakeRequest(POST, s"/analyze/${processor.name}")
      .withFormUrlEncodedBody(filledForm.data.toSeq: _*)
    call(controllers.Analyses.create(processor.name), request)
  }

  def getResultID(taskRequest: Future[Result], retries: Int = 10, wait: Int = 200): Long = {
    val taskUrl = contentAsString(taskRequest)

    val resultUrl = retrieveResultUrl(taskUrl, retries, wait)

    val analysisIDr = "/analyses/([0-9]+)".r
    
    resultUrl.getOrElse("") match {
      case analysisIDr(id) => id.toLong
      case _ => throw new IllegalStateException("No results found")
    }
  }
  
  def retrieveResultUrl(taskUrl: String, retries: Int, wait: Int): Option[String] = {
    var resultUrl: Option[String] = None
    var retriesRemaining = retries
    while (resultUrl.isEmpty && retriesRemaining > 0) {
      Thread.sleep(wait)
      route(FakeRequest(GET, taskUrl)).map { result =>
        resultUrl = redirectLocation(result)
      }
      retriesRemaining -= 1
    }
    resultUrl
  }
}
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

  def retrieveResultUrl(taskUrl: String): Option[String] = {
    var resultUrl: Option[String] = None
    var retries = 10
    while (resultUrl.isEmpty && retries > 0) {
      Thread.sleep(200)
      route(FakeRequest(GET, taskUrl)).map { result =>
        resultUrl = redirectLocation(result)
      }
      retries -= 1
    }
    resultUrl
  }
}
import org.scalatest._
import org.scalatestplus.play._
import org.scalatest.Matchers._

import play.api.libs.json._ 
import play.api.libs.ws._ 
import play.api.test.Helpers._

class ApplicationSpec extends PlaySpec with OneServerPerSuite {
  val myPublicAddress =  s"localhost:$port"
  val corporaEndpoint = s"http://$myPublicAddress/corpora"

  "Application" should {
    "create a corpus" in {
      val corpusJson = Json.obj("name" -> "Testing", "externalID" -> "-1")
      val response = await(WS.url(corporaEndpoint).post(corpusJson))
      response.status shouldBe (CREATED)
    }

    "add texts" in {
    }
  }
}

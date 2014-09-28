import org.scalatest._
import org.scalatestplus.play._
import org.scalatest.Matchers._

import play.api.libs.json._
import play.api.libs.ws._
import play.api.test.Helpers._

class ApplicationSpec extends PlaySpec with OneServerPerSuite {
  val myPublicAddress = s"localhost:$port"
  val corporaEndpoint = s"http://$myPublicAddress/corpora"

  "The Application" should {
    val corpusJson = Json.obj("name" -> "Testing", "externalID" -> "-1")
    val textJson = Json.obj("file-url" -> "", "last-modified" -> "1999-01-01T05:00:00.000Z")

    "create a corpus" in {
      val response = await(WS.url(corporaEndpoint).post(corpusJson))
      response.status shouldBe (CREATED)

      val corpusID = (response.json \ "id").asOpt[Long].getOrElse(fail("No ID returned"))
    }

    "add a text and receive ID in response" in {
      val response = await(WS.url(corporaEndpoint).post(corpusJson))
      response.status should not be (BAD_REQUEST)

      val corpusID = (response.json \ "id").asOpt[Long].getOrElse(fail("No ID returned"))

      val textResponse = await(WS.url(s"$corporaEndpoint/$corpusID/texts").post(textJson))
      textResponse.status should not be (BAD_REQUEST)
      
      val textID = (textResponse.json \ "id").asOpt[Long].getOrElse(fail("No text ID returned"))
    }
  }
}

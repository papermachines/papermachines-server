import org.scalatest._
import org.scalatest.Matchers._
import org.scalatestplus.play._

class HtmlIntegrationSpec extends PlaySpec with OneServerPerSuite with OneBrowserPerSuite with HtmlUnitFactory {
  "The Corpora route" should {
    "return a list of corpora" in {
      go to (s"http://localhost:$port/corpora")
      pageTitle shouldBe "Index of Corpora"
    }

    "allow to add texts" in {

    }
  }
}
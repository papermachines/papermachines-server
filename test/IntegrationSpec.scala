import org.scalatest._
import org.scalatestplus.play._

class ExampleSpec extends PlaySpec with OneServerPerSuite with OneBrowserPerSuite with HtmlUnitFactory {
  "The Corpora route" should {
    "return a list of corpora" in {
      go to (s"http://localhost:$port/corpora")
    }
  }
}
package processors

import org.scalatest._
import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import processors._
import controllers.Analyses
import actors.SpecWithActors
import scala.concurrent.Future
import play.api.mvc.Result
import scala.util.Try
import play.api.libs.json._
import org.chrisjr.topic_annotator.corpora._
import java.net.URI
import NGramProcessor.Gram
import scala.collection.immutable.Queue


class NgramSpec extends SpecWithActors with models.CorpusFixture with AnalysesCommon with TryValues {
  val processor = NGramProcessor

  "The NGramProcessor" should {
    "count n-length sequence frequencies" in {
      val params = processor.ProcessRequest(corpusID, 2)
      var doc = Document.fromString(new URI("nourl"), "a b c d").get
      (Set()++processor.genGrams(List(doc), 2)) mustEqual Set(Gram(Queue("a", "b"),1), Gram(Queue("b", "c"),1),Gram(Queue("c", "d"),1))
      
      doc = Document.fromString(new URI("nourl"), "a b c b c").get
      (Set()++processor.genGrams(List(doc), 2)) mustEqual Set(Gram(Queue("a", "b"),1), Gram(Queue("b", "c"),2),Gram(Queue("c", "b"),1))
      
      doc = Document.fromString(new URI("nourl"), "a b b a b").get
      (Set()++processor.genGrams(List(doc), 2)) mustEqual Set(Gram(Queue("a", "b"),2), Gram(Queue("b", "b"),1),Gram(Queue("b", "a"),1))
      
      doc = Document.fromString(new URI("nourl"), "one two three one two").get
      (Set()++processor.genGrams(List(doc), 2)) mustEqual Set(Gram(Queue("one", "two"),2), Gram(Queue("two", "three"),1),Gram(Queue("three", "one"),1))
      
      
    }
  }
}
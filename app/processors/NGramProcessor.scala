package processors

import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import akka.actor.ActorRef
import scala.collection.GenSeq
import org.chrisjr.topic_annotator.corpora._
import scala.collection.immutable.Queue
import scala.collection.mutable.PriorityQueue

object NGramProcessor extends Processor{
  val defaultTop=10
  val defaultN=2
	val name: String = "n-gram"

	case class ProcessRequest(corpusID: Long, n: Int=defaultN, top: Int=defaultTop) extends Request
	
	val form: Form[ProcessRequest] = Form(
			mapping("corpusID" -> longNumber, "n"->number, "top"->number)
			(ProcessRequest.apply)(ProcessRequest.unapply))
			
	implicit val jsonFormat: Format[ProcessRequest] = Json.format[ProcessRequest]
	case class Gram(words: Seq[String], freq: Int) extends Ordered[Gram] {
	  import scala.math.Ordered.orderingToOrdered
		def compare(that: Gram): Int = this.freq compare that.freq
		def toJS() : JsObject = Json.obj(
		  "Gram" -> Json.toJson(words.mkString),
		  "Freq" -> Json.toJson(freq)
		)  
	}
	
	def genGrams(docs: GenSeq[org.chrisjr.topic_annotator.corpora.Document], n: Int=defaultN, top: Int=defaultTop): Seq[Gram] = {
		var res = Map[Seq[String], Int]().withDefaultValue(0)
		var curr = Queue[String]()
		for(doc <- docs){
			val tokens = doc.tokens.map(_.string)
			for (gram <- tokens.iterator.sliding(n)) {
			  res = res.updated(gram, res(gram) + 1)
			}
		}
		val r = res.toList.map { a => Gram(a._1, a._2) }
		val p = PriorityQueue[Gram]() ++ r
		p.takeRight(top).toSeq
	}
		object NGramAnalyzer extends actors.CorpusAnalyzer[Result] {
			import models.CorpusImplicits._
			val analysisType: String = name

			def resultFrom(n: Int, top: Int, docs: GenSeq[org.chrisjr.topic_annotator.corpora.Document]): JsObject = {
					val ngrams = genGrams(docs, n, top)
					return JsObject(
					    List(("ngrams",JsArray(ngrams.map(x=>x.toJS())))))
			}

			type Params = ProcessRequest
			val preprocessors = Seq(
				LowercaseTransformer
			)

			def makeF(p: Params) = {
				{ corpus =>
					withDB { implicit s =>
						val tokenized = corpus.transform(preprocessors) // implicit conversion to topic-annotator corpus
						resultFrom(p.n, p.top, tokenized.documents.seq)
					}
				}
			}

		// rest is boilerplate and will hopefully be eliminated

		def parseJson(p: JsObject): Params = {
				params = Some(p)
						p.as[ProcessRequest]
		}

		class WorkerImpl(f: F) extends Worker(f: F)
		class CoordinatorImpl(replyTo: ActorRef, f: F) extends Coordinator(replyTo, classOf[WorkerImpl], f)
		val coordinatorClass = classOf[CoordinatorImpl]
	}
	val analyzer = NGramAnalyzer
}
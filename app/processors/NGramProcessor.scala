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
	val name: String = "n-gram"

	case class ProcessRequest(corpusID: Long, n: Int) extends Request
	
	val form: Form[ProcessRequest] = Form(
			mapping("corpusID" -> longNumber, "n"->number)
			(ProcessRequest.apply)(ProcessRequest.unapply))
			
	implicit val jsonFormat: Format[ProcessRequest] = Json.format[ProcessRequest]
	case class Gram(words: Queue[String], freq: Int) extends Ordered[Gram] {
	  import scala.math.Ordered.orderingToOrdered
		def compare(that: Gram): Int = this.freq compare that.freq
		def toJS():JsObject = new JsObject(List(("Gram",JsString(words.fold(""){(x, y)=>x++y})), ("Freq", JsNumber(freq))))
	}
	
	def genGrams(docs: GenSeq[org.chrisjr.topic_annotator.corpora.Document], n: Int): List[Gram] = {
		var res = Map[Queue[String], Int]()
		var curr = Queue[String]()
		for(doc:org.chrisjr.topic_annotator.corpora.Document <- docs){
			val tokens = doc.tokens.map(_.string)
			for (t:String<-tokens){
			  curr=curr.enqueue(t)
				if (curr.length==n){
					(res get curr) match{
						case None => res+=(curr -> 1)
						case Some(x) => res+=(curr -> (x+1))
					}
					var (e:String, curr1:Queue[String])=curr.dequeue
					curr=curr1
				}
			}
		}
		var p = PriorityQueue[Gram]()
		var r = res.toList.map(a=>Gram.apply(a._1, a._2))
		p++=r
		var ret = List[Gram]()
		for(a<-1 to 10){
		  if (!p.isEmpty){
		  	ret:+=p.dequeue()
		  }
		 }
		return ret
	}
		object NGramAnalyzer extends actors.CorpusAnalyzer[Result] {
			import models.CorpusImplicits._
			val analysisType: String = name

			def resultFrom(n: Int = 2, docs: GenSeq[org.chrisjr.topic_annotator.corpora.Document]): JsObject = {
					val ngrams = genGrams(docs, n)
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
						resultFrom(p.n, tokenized.documents.seq)
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
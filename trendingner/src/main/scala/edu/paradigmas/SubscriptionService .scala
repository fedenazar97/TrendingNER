package edu.paradigmas
import scala.collection.mutable.ListBuffer

class SubscriptionService {
  
  case class URL(url:String, parserUrl: GetContent)
  
  var subscribedUrls: ListBuffer[URL] = ListBuffer[URL]()

  def subscribe(url: String, urlParams: List[String], requester: GetContent) {
    subscribedUrls = subscribedUrls ++ urlParams.map { param: String =>
      new URL(url.format(param), requester)
    }.toSeq
  }
  
  def getFeed: Seq[String] = {
    subscribedUrls.map { subs: URL =>
      subs.parserUrl.getContent(subs.url)
    }.flatten.toSeq
  }
}
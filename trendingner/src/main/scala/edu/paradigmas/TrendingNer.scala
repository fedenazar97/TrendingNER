package edu.paradigmas

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.io._
import scala.util.{Try,Success,Failure}
import scala.xml.XML

/*
 * Main class
 */

 object TrendingNer extends App {

  val subscriptionsFilePath: String = "./subscriptions.json"

  case class Subscription(url:String, urlParams:List[String], urlType:String)
  
  def readSubscriptions: List[Subscription] = {
    implicit val formats = DefaultFormats
    val json_content = Source.fromFile(subscriptionsFilePath).mkString

    parse(json_content).extract[List[Subscription]]
  }

  def readFeeds: Seq[String] = {
    val service = new SubscriptionService
    readSubscriptions.foreach {  subs : Subscription =>
      subs.urlType match {
            case "reddit" => service.subscribe(subs.url, subs.urlParams, new GetContentJson)
            case "rss" => service.subscribe(subs.url, subs.urlParams, new GetContentXml)
        }
    }
    val feedTexts = service.getFeed
    feedTexts
  }

  val articlesContent: Seq[String] = readFeeds
 
  // *********************** APPLY THE MODEL ***************************

  val model = new ModelNer
  val extractedNEs: Seq[Seq[String]] = articlesContent.map(model.getNEsSingle)

  (articlesContent zip extractedNEs).foreach {
    case (article, namedEntities) => {
      println("*********************************")
      println(article)
      println(namedEntities)
      println("*********************************")
    }
  }

  // ****************** COUNT AND SORT THE ENTITIES ************************
  val neCounts: Map[String, Int] = extractedNEs.flatten
    .foldLeft(Map.empty[String, Int]) {
      (count, word) => count + (word -> (count.getOrElse(word, 0) + 1)) }
  val sortedNEs: List[(String, Int)] = neCounts.toList
    .sortBy(_._2)(Ordering[Int].reverse)

  sortedNEs.foreach {
    case (namedEntity, count) => {
      println(s"$namedEntity : $count")
    }
  }
}




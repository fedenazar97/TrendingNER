package edu.paradigmas

import scalaj.http.{Http, HttpResponse}
import scala.xml.XML
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.util.matching.Regex

/*
 *
 */
 
abstract class GetContent {
  
  def getContent(url: String) : Seq[String]
  
  def getBody(url: String) = {
    val response:Option[String] = try {  
      Some(Http(url)
      .timeout(connTimeoutMs = 2000, readTimeoutMs = 5000)
      .asString
      .body)
    } catch {
      case e: Exception => {
        None
      }
    }
    response.getOrElse("")
  }
}

class GetContentJson extends GetContent {

  implicit val formats = DefaultFormats
  
  def parserJson(body: String) = {
    val urlRE = "(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]".r
    
    val jsonParse = try {
      Some((parse(body) \ "data" \ "children" \ "data")
        .extract[List[Map[String, Any]]])
    } catch {
      case e : Exception => {
        None
      }
    }
    val parcialresult = jsonParse.getOrElse(Nil).flatten.groupBy(_._1).mapValues(_.map(_._2))
      .filter(x => x._1 == "title" || (x._1 == "selftext" && (x._2.length > 300)))
    
    
    val result = parcialresult.values.flatten.toSeq.map(x => x.toString)
      .map(x => urlRE replaceAllIn(x, ""))

    result
    
  }
  
  def getContent(url: String): Seq[String] = {
    parserJson(getBody(url))
  }
}

class GetContentXml extends GetContent {
 
  def parserXml(body: String): Seq[String] = {
    // Step 2: parse the XML in the response body
    val xmlString = body
    // convert the `String` to a `scala.xml.Elem`
    val xml = try {
      Some(XML.loadString(xmlString))
    } catch {
      case e : Exception => {
        None
      }
    }
  // Extract text from title and description
    val articlesContent: Option[Seq[String]] = try {
      Some((xml.getOrElse(null) \\ "item").map {
        item => ((item \ "title").text + " " + (item \ "description").text)
      })
    } catch {
      case e : Exception => {
        None
      }
    } 
    // To see what's inside articlesContent, use the following line
    // articlesContent.foreach { println }
    articlesContent.getOrElse(Nil)
  }

  def getContent(url: String): Seq[String] = {
    parserXml(getBody(url))
  }
}


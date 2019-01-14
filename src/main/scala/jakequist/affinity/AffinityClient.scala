package jakequist.affinity

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.google.common.io.BaseEncoding
import com.google.common.util.concurrent.RateLimiter
import javax.xml.ws.http.HTTPException
import play.api.http.{HeaderNames, MimeTypes}
import play.api.libs.json.JsResult.Exception
import play.api.libs.json.Json
import play.api.libs.ws.{BodyWritable, WSClient}
import play.api.libs.ws.ahc.{AhcWSClient, StandaloneAhcWSClient}
import play.api.libs.json.Json._
import jakequist.affinity.util.AuxJson._

import scala.concurrent.{ExecutionContext, Future}


case class AffinityException(str: String) extends java.lang.Exception(str)

class AffinityClient(apiKey: String)(implicit ws: WSClient, exc: ExecutionContext) {

  import com.netaporter.uri.dsl._

  val API_BASE = "https://api.affinity.co"
  val RATE_LIMIT_PER_DAY: Double = 150000.0
  val RATE_LIMIT_PER_SECOND: Double = RATE_LIMIT_PER_DAY / (60 * 60 * 24.0)

  val rateLimiter = RateLimiter.create(RATE_LIMIT_PER_SECOND)


  def getLists: Future[Seq[List]] = {
    execGet("/lists").map(_.as[Seq[List]])
  }

  def getList(id: Int) = {
    execGet("/lists" / id.toString).map(_.as[List])
  }

  def getListEntries(listId: Int) = {
    execGet("/lists" / listId.toString / "list-entries").map(_.as[Seq[ListEntry]])
  }

  def getListEntry(listId: Int, entryId: Int) = {
    execGet("/lists" / listId.toString / "list-entries" / entryId.toString).map(_.as[ListEntry])
  }

  def createListEntry(listId: Int, entityId: Int, creatorId: Option[Int] = None) = {
    execPost(
      "/lists" / listId.toString / "list-entries",
      Json.obj("entity_id" -> entityId, "creator_id" -> creatorId)
    )
  }

  def deleteListEntry(listId: Int, entryId: Int) = {
    execDelete("/lists" / listId.toString / "list-entries" / entryId.toString)
  }


  def searchPeople(term: Option[String] = None, pageToken: Option[String] = None) = {
    var url: String = "/persons"
    if (term.isDefined) url = url ? s"term=${term.get}"
    if (pageToken.isDefined) url = url ? s"page_token=${pageToken.get}"
    execGet(url).map(_.as[PersonSearchResponse])
  }

  def listPeople(pageToken : Option[String] = None) = {
    searchPeople(pageToken = pageToken)
  }

  def getPerson(id: Int) = {
    execGet("/persons" / id.toString ? ("with_interaction_dates=true")).map(_.as[Person])
  }

  def createPerson(
                    first_name: String,
                    last_name: String,
                    emails: Seq[String],
                    phone_numbers: Seq[String] = Nil,
                    org_ids: Seq[Int] = Nil
                  ) = {
    var body = Json.obj(
      "first_name" -> first_name,
      "last_name" -> last_name,
      "emails" -> emails
    )
    if (phone_numbers.nonEmpty) body ++= Json.obj("phone_numbers" -> phone_numbers)
    if (org_ids.nonEmpty) body ++= Json.obj("organization_ids" -> org_ids)
    execPost("/persons", body).map(_.as[Entity])
  }


  def getPersonFields = {
    execGet("/persons/fields").map(_.as[Seq[Field])
  }

  def getRelationshipStrengths(internal_id: Option[Int] = None, external_id: Option[Int] = None) = {
    var url: String = "/relationship-strengths"
    if (internal_id.isDefined) url = url ? s"internal_id=${internal_id.get}"
    if (external_id.isDefined) url = url ? s"external_id=${external_id.get}"
    execGet(url).map(_.as[Seq[RelationshipStrength]])
  }



  def searchOrg(term: Option[String] = None, pageToken: Option[String] = None) = {
    var url: String = "/organizations"
    if (term.isDefined) url = url ? s"term=${term.get}"
    if (pageToken.isDefined) url = url ? s"page_token=${pageToken.get}"
    execGet(url).map(_.as[OrgSearchResponse])
  }

  def getOrg(id: Int) = {
    execGet("/organizations" / id.toString ? ("with_interaction_dates=true")).map(_.as[Org])
  }

  def getOrgFields= {
    execGet("/organizations/fields").map(_.as[Seq[Field])
  }

  








  def close = {
    ws.close()
  }

  private def apiKeyBase64 = {
    BaseEncoding.base64().encode((":" + apiKey).getBytes)
  }

  private def execGet(path: String) = {
    Future(rateLimiter.acquire())
      .flatMap { _ =>
        ws.url(API_BASE / path)
          .withHttpHeaders(
            "Authorization" -> s"Basic ${apiKeyBase64}",
            HeaderNames.ACCEPT -> MimeTypes.JSON
          )
          .withMethod("GET")
          .execute()
      }
      .map { resp =>
        if (resp.status != 200) throw new AffinityException(s"Bad HTTP ${resp.status}: ${resp.statusText}")
        resp.json
      }
  }

  private def execDelete(path: String) = {
    Future(rateLimiter.acquire())
      .flatMap { _ =>
        ws.url(API_BASE / path)
          .withHttpHeaders(
            "Authorization" -> s"Basic ${apiKeyBase64}",
            HeaderNames.ACCEPT -> MimeTypes.JSON
          )
          .withMethod("DELETE")
          .execute()
      }
      .map { resp =>
        if (resp.status != 200) throw new AffinityException(s"Bad HTTP ${resp.status}: ${resp.statusText}")
        resp.json
      }
  }

  private def execPost[T: BodyWritable](path: String, body: T) = {

    Future(rateLimiter.acquire())
      .flatMap { _ =>
        ws.url(API_BASE / path)
          .withHttpHeaders(
            "Authorization" -> s"Basic ${apiKeyBase64}",
            HeaderNames.ACCEPT -> MimeTypes.JSON
          )
          .withBody(body)
          .withMethod("POST")
          .execute()
      }
      .map { resp =>
        if (resp.status != 200) throw new AffinityException(s"Bad HTTP ${resp.status}: ${resp.statusText}")
        resp.json
      }


  }

}


object AffinityClient {

  private implicit lazy val system = ActorSystem("affinity-client")
  private implicit lazy val materializer = ActorMaterializer()
  private lazy val defaultWSClient: WSClient = {
    AhcWSClient()
  }

  def apply(apiKey: String)(implicit ws: WSClient = defaultWSClient, exc: ExecutionContext = ExecutionContext.global): AffinityClient = {
    new AffinityClient(apiKey)
  }

  def shutdown = {
    system.terminate()
  }

}
package com.sgw.problems

import akka.actor.{Actor, Props, ActorSystem}
import akka.event.slf4j.SLF4JLogging
import akka.io.IO
import com.typesafe.config.ConfigFactory
import net.liftweb.json.Serialization._
import spray.can.Http
import spray.http._
import spray.routing.{RequestContext, RejectionHandler, HttpService}

import scala.io.Source
import net.liftweb.json._

import scala.util.{Failure, Success, Try}

/**
 * Holds service configuration settings.
 */
trait Configuration {

  /**
   * Application config object.
   */
  val config = ConfigFactory.load()

  /** Host name/address to start service on. */
  lazy val serviceHost = Try(config.getString("service.host")).getOrElse("localhost")

  /** Port to start service on. */
  lazy val servicePort = Try(config.getInt("service.port")).getOrElse(8080)
}

/**
 * Find the n nearest zip codes to the specified zip code.
 */
object ZipCodes extends App with Configuration {
  lazy val zipCodes: Seq[ZipCode] = fromResource()

  private def fromResource(name: String = "/zips.json"): Seq[ZipCode] = {
    val zipsJSON = Source.fromInputStream(getClass.getResourceAsStream(name)).getLines().mkString("\n")

    implicit val formats = DefaultFormats

    parse(zipsJSON).extract[List[Map[String, String]]]
      .filter(zipMap => zipMap.contains("zip") && zipMap.contains("longitude") && zipMap.contains("latitude"))
      .map(zipMap => ZipCode(
        zipMap.getOrElse("city", ""),
        zipMap("zip"),
        zipMap.getOrElse("dst", ""),
        zipMap("longitude").toDouble,
        zipMap("latitude").toDouble,
        zipMap.getOrElse("state", ""),
        zipMap.getOrElse("timezone", "")
      ))
  }

  def nearest(toZipCodeStr: String, n: Int): Seq[ZipCode] = {
    val toZipCode = zipCodes
      .find(zipCode => zipCode.zipCode == toZipCodeStr)
      .getOrElse(throw new RuntimeException("Invalid zip code '$zipCode'!"))

    nearest(toZipCode, n)
  }

  def nearest(zipCode: ZipCode, n: Int): Seq[ZipCode] =
    zipCodes.sortBy(zipCode.distance).drop(1).take(n)

  // create an actor system for application
  implicit val system = ActorSystem("notification-service")

  // create and start rest service actor
  private val restService = system.actorOf(Props[RestServiceActor], "rest-endpoint")

  // start HTTP server with rest service actor as a handler
  IO(Http) ! Http.Bind(restService, serviceHost, servicePort)
}

/**
 * REST Service actor.
 */
class RestServiceActor extends Actor with NotificationRestService {

  implicit def actorRefFactory = context

  def receive = runRoute(rest)
}

/**
 * REST Service
 */
trait NotificationRestService extends HttpService with SLF4JLogging {
  implicit val executionContext = actorRefFactory.dispatcher

  implicit val liftJsonFormats = DefaultFormats

  implicit val notificationRejectionHandler = RejectionHandler {
    case rejections => mapHttpResponse {
      response =>
        response.withEntity(HttpEntity(ContentType(MediaTypes.`application/json`),
          write(Map("error" -> response.entity.asString))))
    } {
      RejectionHandler.Default(rejections)
    }
  }

  val rest = respondWithMediaType(MediaTypes.`application/json`) {
    path(Segment / IntNumber) {
      case (zipCodeStr, n) =>
        get {
          ctx: RequestContext => handleRequest(ctx) {
            log.debug(s"Searching for the $n closest zip codes to $zipCodeStr.")
            Try {
              val nearestZipCodes = ZipCodes.nearest(zipCodeStr, n)
              nearestZipCodes.foreach(println)
              nearestZipCodes
            }
          }
        }
    }
  }

  /**
   * Handles an incoming request and create valid response for it.
   *
   * @param ctx         request context
   * @param successCode HTTP Status code for success
   * @param action      action to perform
   */
  protected def handleRequest(ctx: RequestContext, successCode: StatusCode = StatusCodes.OK)(action: => Try[Seq[ZipCode]]) {
    action match {
      case Success(result: Object) =>
        ctx.complete(successCode, write(result))
      case Failure(t) => {
        t.printStackTrace()
        ctx.complete(StatusCodes.InternalServerError, net.liftweb.json.Serialization.write(Map("error" -> t.getMessage)))
      }
    }
  }
}

case class ZipCode(city: String, zipCode: String, dst: String, longitude: Double, latitude: Double, state: String, timezone: String) {
  private val R = 6371000 // metres

  def distance(otherZipCode: ZipCode): Double = distance(latitude, longitude, otherZipCode.latitude, otherZipCode.longitude)

  private def toRadians(degrees: Double): Double = degrees * Math.PI / 180.0

  private def distance(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {
    val rad1 = toRadians(lat1)
    val rad2 = toRadians(lat2)
    val latRadDiff = toRadians(lat2 - lat1)
    val lonRadDiff = toRadians(lon2 - lon1)

    val a = Math.sin(latRadDiff / 2) * Math.sin(lonRadDiff / 2) +
      Math.cos(rad1) * Math.cos(rad2) *
        Math.sin(lonRadDiff / 2) * Math.sin(lonRadDiff / 2)
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))

    R * c
  }
}


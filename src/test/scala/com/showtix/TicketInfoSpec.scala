package com.showtix
import org.joda.time.{DateTime, Duration}
import org.scalatest.{MustMatchers, WordSpec}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class TicketInfoSpec extends WordSpec with MustMatchers {
  object TicketInfoService extends TicketInfoService with MockWebServiceCalls {
    "getTicketInfo" must {
      "return a complete ticket info when all futures are successful" in {
        val ticketInfo = Await.result(getTicketInfo("254", Location(1d, 2d)), 300.millis)

        ticketInfo.event.isEmpty must be(false)
        ticketInfo.event.foreach(event ⇒ event.name must be("Coachella"))
        ticketInfo.travelAdvice.isEmpty must be(false)
        ticketInfo.suggestions.map(_.name) must be(Seq("Fvzzkill", "MadLib", "Flying Lotus"))
      }
    }
    "return an incomplete ticket info when getEvent fails" in {
      val ticketInfo = Await.result(getTicketInfo("1234", Location(1d, 2d)), 300.millis)
      ticketInfo.event.isEmpty must be(true)
      ticketInfo.travelAdvice.isEmpty must be(true)
      ticketInfo.suggestions.isEmpty must be(true)
    }
  }

}

trait MockWebServiceCalls extends WebServiceCalls {
  import scala.concurrent.ExecutionContext.Implicits.global

  def getEvent(ticketNr: String, location: Location): Future[TicketInfo] = {
    Future {
      if (ticketNr == "254") {
        TicketInfo(ticketNr, location, event = Some(Event("Coachella", Location(33.7206, 116.2156), DateTime.now())))
      } else throw new Exception("oh! crap")
    }
  }

  def callWeatherXService(ticketInfo: TicketInfo): Future[Option[Weather]] = {
    Future { Some(Weather(30, precipitation = false))}
  }

  def callWeatherYService(ticketInfo: TicketInfo): Future[Option[Weather]] = {
    Future { Some(Weather(30, precipitation = false))}
  }

  def callTrafficService(origin: Location, destination: Location, time: DateTime): Future[Option[RouteByCar]] = {
    Future {
      Some(RouteByCar("route1", DateTime.now().minusMinutes(35), origin, destination,
        new Duration(30L), new Duration(20L)))
    }
  }

  def callPublicTransportService(origin: Location, destination: Location, time: DateTime): Future[Option[PublicTransportAdvice]] = {
    Future {
      Some(PublicTransportAdvice("public transport route 1", DateTime.now().minusMinutes(20), origin, destination,
        new Duration(20L)))
    }
  }

  def callSimilarArtistService(event: Event): Future[Seq[Artist]] = {
    Future {
      Seq(Artist("Fvzzkill", "iamfuzzkill.com/calendar"), Artist("MadLib", "madlib.com/calendar"),
        Artist("Flying Lotus", "fly.lo/calendar"))
    }
  }

  def callArtistCalendarService(artist: Artist, nearLocation: Location): Future[Event] = {
    Future {
      Event(artist.name, Location(1d, 1d), DateTime.now())
    }
  }
}
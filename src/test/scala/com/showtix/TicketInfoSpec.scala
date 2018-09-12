package com.showtix
import org.joda.time.{DateTime, Duration}

import scala.concurrent.Future

class TicketInfoSpec {

}

trait mockWebServiceCalls extends WebServiceCalls {
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
        new Duration(20L) ))
    }
  }
}
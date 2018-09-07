package com.showtix

import org.joda.time.DateTime

import scala.concurrent.Future
import scala.util.control.NonFatal
trait TicketInfoService extends WebServiceCalls {
  import scala.concurrent.ExecutionContext.Implicits.global

  type Recovery[T] = PartialFunction[Throwable, T]

//  recover with None
  def withNone[T]:Recovery[Option[T]] = { case NonFatal(e) ⇒ None }

//  recovery with empty sequence
  def withEmptySeq[T]:Recovery[Seq[T]] = { case NonFatal(e) ⇒ Seq() }

//  recover with the previous info that was built in the previous step
  def withPrevious(previousTicketInfo: TicketInfo): Recovery[TicketInfo] = { case NonFatal(e) ⇒ previousTicketInfo }

  def getWeather(ticketInfo: TicketInfo): Future[TicketInfo] = {
    val futureWeatherX = callWeatherXService(ticketInfo).recover(withNone)

    val futureWeatherY = callWeatherYService(ticketInfo).recover(withNone)

    val futures: List[Future[Option[Weather]]] = List(futureWeatherX, futureWeatherY)
    val fastestResponse = Future.firstCompletedOf(futures)
    fastestResponse.map{ weatherResponse ⇒
      ticketInfo.copy(weather = weatherResponse)
    }
  }

  def getTraffic(ticketInfo: TicketInfo): Future[TicketInfo] = {
    ticketInfo.event.map { event ⇒
      callTrafficService(ticketInfo.userLocation, event.location, event.time).map {routeResponse ⇒
        ticketInfo.copy(travelAdvice = Some(TravelAdvice(routeByCar = routeResponse)))
      }
    }.getOrElse(Future.successful(ticketInfo))
  }

  def getPublicTransportAdvice(ticketInfo: TicketInfo): Future[TicketInfo] = {
    ticketInfo.event.map { event ⇒
      callPublicTransportService(ticketInfo.userLocation, event.location, event.time).map { publicTransportResponse ⇒
        val newAdvice = ticketInfo.travelAdvice.map(_.copy(publicTransportAdvice =  publicTransportResponse))
        ticketInfo.copy(travelAdvice = newAdvice)
      }.recover(withPrevious(ticketInfo))
    }.getOrElse(Future.successful(ticketInfo))
  }

  def getCarRoute(ticketInfo: TicketInfo): Future[TicketInfo] = {
    ticketInfo.event.map { event ⇒
      callTrafficService(ticketInfo.userLocation, event.location, event.time).map { carRouteAdvice ⇒
        val newAdvice = ticketInfo.travelAdvice.map(_.copy(routeByCar = carRouteAdvice))
        ticketInfo.copy(travelAdvice = newAdvice )
      }.recover(withPrevious(ticketInfo))
    }.getOrElse(Future.successful(ticketInfo))
  }

  def getTravelAdvice(info: TicketInfo, event: Event): Future[TicketInfo] = {
    val futureRoute = callTrafficService(info.userLocation, event.location, event.time).recover(withNone)

    val futurePublicTransport = callPublicTransportService(info.userLocation, event.location, event.time).recover(withNone)

    futureRoute.zip(futurePublicTransport).map { case (routeByCar, publicTransportAdvice) ⇒
    val travelAdvice = TravelAdvice(routeByCar, publicTransportAdvice)
    info.copy(travelAdvice = Some(travelAdvice))
    }
  }

  def getTravelAdviceUsingForComprehension(ticketInfo: TicketInfo, event: Event): Future[TicketInfo] = {
    val futureRoute = callTrafficService(ticketInfo.userLocation, event.location, event.time).recover(withNone)

    val futurePublicTransport = callPublicTransportService(ticketInfo.userLocation, event.location, event.time).recover(withNone)

    for {
      (routeByCar, publicTransportAdvcie) ← futureRoute.zip(futurePublicTransport)
      travelAdvice = TravelAdvice(routeByCar, publicTransportAdvcie)
    } yield ticketInfo.copy(travelAdvice = Some(travelAdvice))
  }

  def getPlannedEvents(event: Event, artists: Seq[Artist]): Future[Seq[Event]]= {
    val events = artists.map { artist ⇒
      callArtistCalendarService(artist, event.location)
    }
    Future.sequence(events)
  }

}

trait WebServiceCalls {
  def callWeatherXService(ticketInfo: TicketInfo): Future[Option[Weather]]

  def callWeatherYService(ticketInfo: TicketInfo): Future[Option[Weather]]

  def callTrafficService(origin: Location, destination: Location, time: DateTime): Future[Option[RouteByCar]]

  def callPublicTransportService(origin: Location, destination: Location, time: DateTime): Future[Option[PublicTransportAdvice]]

  def callArtistCalendarService(artist: Artist, nearLocation: Location): Future[Event]

}

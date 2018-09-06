package com.showtix

import scala.concurrent.Future
import scala.util.control.NonFatal
trait TicketInfoService extends WebServiceCalls {
  import scala.concurrent.ExecutionContext.Implicits.global

  type Recovery[T] = PartialFunction[Throwable, T]

//  recover with None
  def withNone[T]:Recovery[Option[T]] = { case NonFatal(e) ⇒ None }
  def getWeather(ticketInfo: TicketInfo): Future[TicketInfo] = {
    val futureWeatherX = callWeatherXService(ticketInfo).recover(withNone)

    val futureWeatherY = callWeatherYService(ticketInfo).recover(withNone)

    val futures: List[Future[Option[Weather]]] = List(futureWeatherX, futureWeatherY)
    val fastestResponse = Future.firstCompletedOf(futures)
    fastestResponse.map{ weatherResponse ⇒
      ticketInfo.copy(weather = weatherResponse)
    }
  }

}

trait WebServiceCalls {
  def callWeatherXService(ticketInfo: TicketInfo): Future[Option[Weather]]
  def callWeatherYService(ticketInfo: TicketInfo): Future[Option[Weather]]
}

package com.showtix
import org.joda.time.DateTime

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
}
package com.showtix

import org.joda.time.{DateTime, Duration}

case class TicketInfo(ticketNumber: String,
                      userLocation: Location,
                      event: Option[Event] = None,
                      travelAdvice: Option[TravelAdvice] = None,
                      weather: Option[Weather] = None,
                      suggestions: Seq[Event] = Seq())

case class Event(name: String, location: Location, time: DateTime)

case class Weather(temperature: Int, precipitation: Boolean)

case class RouteByCar(route: String,
                      timeToLeave: DateTime,
                      origin: Location,
                      destination: Location,
                      estimatedDuration: Duration,
                      trafficJamTime: Duration)

case class Location(latitude: Double, longitude: Double)

case class TravelAdvice(routeByCar: Option[RouteByCar] = None,
                        publicTransportAdvice: Option[PublicTransportAdvice] = None)

case class PublicTransportAdvice(advice: String,
                                 timeToLeave: DateTime,
                                 origin: Location,
                                 destination: Location,
                                 estimatedDuration: Duration)

case class Artist(name: String, calendarUri: String)


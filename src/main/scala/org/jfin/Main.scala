package org.jfin

import java.time.LocalDate

object Main extends App {
  val calendar = SettlementCalendars.gb
  println(calendar.holiday(LocalDate.of(2018,4,2)))
}

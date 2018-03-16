package org.jfin

import java.time.LocalDate

object Main extends App {
  val calendar = SettlementCalendars.gb

  for (r <- calendar.rules) {
    println(s"${r.name}: ${r.describe}")
  }

  println(calendar.holiday(LocalDate.of(2018,4,2)))
}

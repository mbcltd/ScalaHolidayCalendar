package org.jfin

import java.time.LocalDate


case class HolidayCalendar(rules:List[HolidayRule], locale:String) {
  def isHoliday(d:LocalDate): Boolean = holiday(d).isDefined
  def holiday(d:LocalDate): Option[Holiday] = rules.flatMap( _.isHoliday(d) ).headOption
  def allHolidaysForYear(year:Int):List[Holiday] = {
    val firstJan = LocalDate.of(year,1,1)
    val days = (1 to firstJan.lengthOfYear()).map( LocalDate.ofYearDay(year, _) )
    days.flatMap( holiday ).toList
  }
}

case class Holiday(date:LocalDate, rule:HolidayRule) {
  def name:String = rule.name
}


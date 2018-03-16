package org.jfin

import java.time.LocalDate


case class HolidayCalendar(rules:List[HolidayRule], locale:String) {
  def isHoliday(d:LocalDate): Boolean = holiday(d).isDefined
  def holiday(d:LocalDate): Option[Holiday] = rules.flatMap( _.isHoliday(d) ).headOption
}

case class Holiday(rule:HolidayRule) {
  def name:String = rule.name
}


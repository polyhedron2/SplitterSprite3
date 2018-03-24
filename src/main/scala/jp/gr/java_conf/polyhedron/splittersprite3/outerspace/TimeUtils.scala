package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import java.util.{Locale}

import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.{DateTimeFormat}

abstract class TimeUtils {
  val timeZone: DateTimeZone
  def currentTimeMillis: Long

  //yyyy-MM-dd aaa HH:mm:ss.sss tz
  def currentTimeMillisStr: String = {
    val dateTime = new DateTime(currentTimeMillis, timeZone)
    DateTimeFormat.forPattern("yyyy-MM-dd EEE HH:mm:ss.SSS ZZ")
      .withLocale(Locale.US).print(dateTime)
  }
}

package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.{DateTimeFormat}

abstract class TimeUtils {
  val timeZone: DateTimeZone
  def currentTimeMillis: Long

  //yyyy-MM-dd aaa HH:mm:ss.sss tz
  def currentTimeMillisStr: String = {
    val dateTime = new DateTime(currentTimeMillis, timeZone)
    DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss").print(dateTime)
  }
}

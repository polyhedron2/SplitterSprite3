package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import org.joda.time.{DateTimeZone}

class ProductionTimeUtils() extends TimeUtils {
  override val timeZone = DateTimeZone.getDefault()
  override def currentTimeMillis: Long = System.currentTimeMillis()
}

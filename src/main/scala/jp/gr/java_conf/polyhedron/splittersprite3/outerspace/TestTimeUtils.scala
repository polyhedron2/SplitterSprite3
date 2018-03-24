package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import org.joda.time.{DateTimeZone}

object TestTimeUtils {
  class TooManyCurrentMillisException() extends Exception()
  class TooFewCurrentMillisException() extends Exception()
}

class TestTimeUtils(var currentTimeMillisList: List[Long], timeZoneID: String)
    extends TimeUtils {
  override val timeZone = DateTimeZone.forID(timeZoneID)

  override def currentTimeMillis: Long = currentTimeMillisList match {
    case head :: tail => {
      currentTimeMillisList = tail
      head
    }
    case Nil => throw new TestTimeUtils.TooFewCurrentMillisException()
  }
}

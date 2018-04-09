package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import org.joda.time.{DateTimeZone}

import jp.gr.java_conf.polyhedron.splittersprite3.agent

object TestTimeUtils {
  class TooManyCurrentTimeMillisException() extends Exception()
    with agent.LoanAgentGoingThroughException
  class TooFewCurrentTimeMillisException() extends Exception()
    with agent.LoanAgentGoingThroughException
}

class TestTimeUtils(var currentTimeMillisList: List[Long], timeZoneID: String)
    extends TimeUtils {
  override val timeZone = DateTimeZone.forID(timeZoneID)

  override def currentTimeMillis: Long = synchronized {
    currentTimeMillisList match {
      case head :: tail => {
        currentTimeMillisList = tail
        head
      }
      case Nil => throw new TestTimeUtils.TooFewCurrentTimeMillisException()
    }
  }
}

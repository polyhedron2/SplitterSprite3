import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}

import jp.gr.java_conf.polyhedron.splittersprite3.Atmosphere
import jp.gr.java_conf.polyhedron.splittersprite3.agent
import jp.gr.java_conf.polyhedron.splittersprite3.outerspace

class TimeUtilsSpec extends FlatSpec with DiagrammedAssertions with Matchers {
  "TestTimeUtils" should "少なすぎる引数で例外" in {
    val e = intercept[
        outerspace.TestTimeUtils.TooFewCurrentTimeMillisException] {
      Atmosphere.withTestIOUtils {
        Atmosphere.withTestTimeUtils(List()) {
          // Logger起動時のcurrentTimeMillis１回が実行される
          agent.LoanAgent.loan { }
        }
      }
    }
  }

  "TestTimeUtils" should "多すぎる引数で例外" in {
    val e = intercept[
        outerspace.TestTimeUtils.TooManyCurrentTimeMillisException] {
      Atmosphere.withTestIOUtils {
        Atmosphere.withTestTimeUtils(List(0, 1)) {
          // Logger起動時のcurrentTimeMillis１回が実行される
          agent.LoanAgent.loan { }
        }
      }
    }
  }

  val currentTimeMillisList = List(
    0L, 1L, 10L, 100L, // millisec
    1000L, 10 * 1000L, // seconds
    60 * 1000L, 10 * 60 * 1000L, // minutes
    60 * 60 * 1000L, 10 * 60 * 60 * 1000L, // hours
    1 * 24 * 60 * 60 * 1000L, // Fri
    2 * 24 * 60 * 60 * 1000L, // Sat
    3 * 24 * 60 * 60 * 1000L, // Sun
    4 * 24 * 60 * 60 * 1000L, // Mon
    5 * 24 * 60 * 60 * 1000L, // Tue
    6 * 24 * 60 * 60 * 1000L, // Wed
    7 * 24 * 60 * 60 * 1000L, // Thu
    31 * 24 * 60 * 60 * 1000L, // 1970-02-01
    365 * 24 * 60 * 60 * 1000L, // 1971-01-01
    100000000000L, //1973-03-03T09:46:40
    1000000000000L, //2001-09-09T01:46:40
    1234567890000L, //2009-02-13T23:31:30
    2147483647000L, //2038-01-19T03:14:07
    2147483648000L, //2038-01-19T03:14:08
  )

  "TimeUtils" should "現在時刻を指定形式で出力" in {
    Atmosphere.withTestTimeUtils(currentTimeMillisList, "UTC") {
      val expectedStrList = List(
        "1970-01-01 Thu 00:00:00.000 +00:00",
        "1970-01-01 Thu 00:00:00.001 +00:00",
        "1970-01-01 Thu 00:00:00.010 +00:00",
        "1970-01-01 Thu 00:00:00.100 +00:00",
        "1970-01-01 Thu 00:00:01.000 +00:00",
        "1970-01-01 Thu 00:00:10.000 +00:00",
        "1970-01-01 Thu 00:01:00.000 +00:00",
        "1970-01-01 Thu 00:10:00.000 +00:00",
        "1970-01-01 Thu 01:00:00.000 +00:00",
        "1970-01-01 Thu 10:00:00.000 +00:00",
        "1970-01-02 Fri 00:00:00.000 +00:00",
        "1970-01-03 Sat 00:00:00.000 +00:00",
        "1970-01-04 Sun 00:00:00.000 +00:00",
        "1970-01-05 Mon 00:00:00.000 +00:00",
        "1970-01-06 Tue 00:00:00.000 +00:00",
        "1970-01-07 Wed 00:00:00.000 +00:00",
        "1970-01-08 Thu 00:00:00.000 +00:00",
        "1970-02-01 Sun 00:00:00.000 +00:00",
        "1971-01-01 Fri 00:00:00.000 +00:00",
        "1973-03-03 Sat 09:46:40.000 +00:00",
        "2001-09-09 Sun 01:46:40.000 +00:00",
        "2009-02-13 Fri 23:31:30.000 +00:00",
        "2038-01-19 Tue 03:14:07.000 +00:00",
        "2038-01-19 Tue 03:14:08.000 +00:00",
      )
      expectedStrList.foreach(expectedStr =>
        Atmosphere.timeUtils.currentTimeMillisStr should be (expectedStr))
    }
  }

  "TimeUtils" should "日本のタイムゾーンでも現在時刻を指定形式で出力" in {
    Atmosphere.withTestTimeUtils(currentTimeMillisList, "Asia/Tokyo") {
      val expectedStrList = List(
        "1970-01-01 Thu 09:00:00.000 +09:00",
        "1970-01-01 Thu 09:00:00.001 +09:00",
        "1970-01-01 Thu 09:00:00.010 +09:00",
        "1970-01-01 Thu 09:00:00.100 +09:00",
        "1970-01-01 Thu 09:00:01.000 +09:00",
        "1970-01-01 Thu 09:00:10.000 +09:00",
        "1970-01-01 Thu 09:01:00.000 +09:00",
        "1970-01-01 Thu 09:10:00.000 +09:00",
        "1970-01-01 Thu 10:00:00.000 +09:00",
        "1970-01-01 Thu 19:00:00.000 +09:00",
        "1970-01-02 Fri 09:00:00.000 +09:00",
        "1970-01-03 Sat 09:00:00.000 +09:00",
        "1970-01-04 Sun 09:00:00.000 +09:00",
        "1970-01-05 Mon 09:00:00.000 +09:00",
        "1970-01-06 Tue 09:00:00.000 +09:00",
        "1970-01-07 Wed 09:00:00.000 +09:00",
        "1970-01-08 Thu 09:00:00.000 +09:00",
        "1970-02-01 Sun 09:00:00.000 +09:00",
        "1971-01-01 Fri 09:00:00.000 +09:00",
        "1973-03-03 Sat 18:46:40.000 +09:00",
        "2001-09-09 Sun 10:46:40.000 +09:00",
        "2009-02-14 Sat 08:31:30.000 +09:00",
        "2038-01-19 Tue 12:14:07.000 +09:00",
        "2038-01-19 Tue 12:14:08.000 +09:00",
      )
      expectedStrList.foreach(expectedStr =>
        Atmosphere.timeUtils.currentTimeMillisStr should be (expectedStr))
    }
  }
}

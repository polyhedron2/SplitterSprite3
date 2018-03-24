import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}
import java.nio.file.{Files}

import jp.gr.java_conf.polyhedron.splittersprite3.Atmosphere
import jp.gr.java_conf.polyhedron.splittersprite3.agent

class LoggerSpec extends FlatSpec with DiagrammedAssertions with Matchers {
  "Logger" should "ログレベルに応じたログを出力" in {
    def assertLogLevel(
        logLevel: agent.Logger.LogLevel,
        expectedLines: List[String], unexpectedLines: List[String]) {
      val testIOUtils = Atmosphere.withTestIOUtils {
        val success = agent.LoanAgent.loan {
          val prevLogLevel = agent.Logger.logLevel
          agent.Logger.logLevel = logLevel
          try {
            agent.Logger.traceLog("trace line")
            agent.Logger.debugLog("debug line")
            agent.Logger.infoLog("info line")
            agent.Logger.warnLog("warn line")
            agent.Logger.errorLog("error line")
            agent.Logger.fatalLog("fatal line")
          } finally {
            agent.Logger.logLevel = prevLogLevel
          }
        }
        assert(success)
      }
      val error = testIOUtils.dumpStdErr()
      expectedLines.foreach(error should include (_))
      unexpectedLines.foreach(error should not include (_))
    }

    assertLogLevel(agent.Logger.Trace, List(
      "trace line",
      "debug line",
      "info line",
      "warn line",
      "error line",
      "fatal line",
    ), List(
    ))

    assertLogLevel(agent.Logger.Debug, List(
      "debug line",
      "info line",
      "warn line",
      "error line",
      "fatal line",
    ), List(
      "trace line",
    ))

    assertLogLevel(agent.Logger.Info, List(
      "info line",
      "warn line",
      "error line",
      "fatal line",
    ), List(
      "trace line",
      "debug line",
    ))

    assertLogLevel(agent.Logger.Warn, List(
      "warn line",
      "error line",
      "fatal line",
    ), List(
      "trace line",
      "debug line",
      "info line",
    ))

    assertLogLevel(agent.Logger.Error, List(
      "error line",
      "fatal line",
    ), List(
      "trace line",
      "debug line",
      "info line",
      "warn line",
    ))

    assertLogLevel(agent.Logger.Fatal, List(
      "fatal line",
    ), List(
      "trace line",
      "debug line",
      "info line",
      "warn line",
      "error line",
    ))
  }

  "Logger" should "処理中に生じた例外はprintStackTrace" in {
    val testIOUtils = Atmosphere.withTestIOUtils {
      val success = agent.LoanAgent.loan {
        throw new Exception("this is uncaught exception")
      }
      assert(!success)
    }
    val error = testIOUtils.dumpStdErr()
    error should include (
      "FATAL: java.lang.Exception: ゲーム実行中にエラーが発生しました")
    error should include (
      "FATAL: Caused by: java.lang.Exception: this is uncaught exception")
  }

  "Logger" should "１０個以上の個数のログファイルは古いものから削除" in {
    Atmosphere.withTestIOUtils {
      Atmosphere.withTestTimeUtils(List(0), "UTC") {
        val logDir = Atmosphere.ioUtils.gameDirPath.resolve("log")

        Files.createDirectories(logDir)
        for (i <- 0 until 11) {
          val logFile = logDir.resolve(f"dummy_${i}%03d.log")
          Files.createFile(logFile)
        }

        Atmosphere.ioUtils.childrenList(logDir).map(
          Atmosphere.ioUtils.tailNameOf).toSet should be (Set(
            "dummy_000.log", "dummy_001.log", "dummy_002.log", "dummy_003.log",
            "dummy_004.log", "dummy_005.log", "dummy_006.log", "dummy_007.log",
            "dummy_008.log", "dummy_009.log", "dummy_010.log"))

        val success = agent.LoanAgent.loan {}
        assert(success)

        Atmosphere.ioUtils.childrenList(logDir).map(
          Atmosphere.ioUtils.tailNameOf).toSet should be (Set(
            "dummy_002.log", "dummy_003.log", "dummy_004.log", "dummy_005.log",
            "dummy_006.log", "dummy_007.log", "dummy_008.log", "dummy_009.log",
            "dummy_010.log", "1970-01-01_Thu_00:00:00.000_+00:00.log"))
      }
    }
  }
}

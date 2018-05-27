import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}

import jp.gr.java_conf.polyhedron.splittersprite3.Atmosphere
import jp.gr.java_conf.polyhedron.splittersprite3.agent

class ThreadPoolSpec extends FlatSpec with DiagrammedAssertions with Matchers {
  val wait_ms = 100
  val wait_ns: Long = wait_ms * 1000 * 1000

  "ThreadPool" should "別スレッドでRunnableを起動" in {
    var checked = false

    val runnable = new agent.ThreadPool.Runnable() {
      override def runOnce() = {
        Thread.sleep(wait_ms)
        checked = true
        // ループ一回で停止
        false
      }
    }

    Atmosphere.withTestIOUtils {
      val success = agent.LoanAgent.loan {
        assert(agent.ThreadPool.size === 0)
        agent.ThreadPool.startAndGetHalter(runnable)
        assert(agent.ThreadPool.size === 1)
        // 変数更新前
        assert(checked === false)
        Thread.sleep(wait_ms * 2)
        // 変数更新済
        assert(agent.ThreadPool.size === 0)
        assert(checked === true)
      }
      assert(success)
    }
  }

  "ThreadPool" should "haltで無限ループ停止" in {
    var count = 0

    val runnable = new agent.ThreadPool.Runnable() {
      override def runOnce() = {
        Thread.sleep(wait_ms)
        count += 1
        // 無限ループ
        true
      }
    }

    Atmosphere.withTestIOUtils {
      val success = agent.LoanAgent.loan {
        assert(count === 0)
        assert(agent.ThreadPool.size === 0)
        val halt = agent.ThreadPool.startAndGetHalter(runnable)
        assert(agent.ThreadPool.size === 1)
        Thread.sleep(wait_ms * 2)
        // 増加中
        assert(count > 0)
        var prevCount = count
        Thread.sleep(wait_ms * 2)
        // 増加中
        assert(agent.ThreadPool.size === 1)
        assert(count > prevCount)

        halt()
        Thread.sleep(wait_ms * 2)
        assert(agent.ThreadPool.size === 0)
        prevCount = count
        Thread.sleep(wait_ms * 2)
        assert(count === prevCount)
      }
      assert(success)
    }
  }

  "ThreadPool" should "実装された前後処理を実行" in {
    var checkList = List[String]()

    val runnable = new agent.ThreadPool.Runnable() {
      override def enter() { checkList :+= "enter" }

      override def runOnce() = {
        Thread.sleep(wait_ms)
        checkList :+= "runOnce"
        // 三回のみ実行
        nextLoopCount < 3
      }

      override def exit() { checkList :+= "exit" }
    }

    Atmosphere.withTestIOUtils {
      val success = agent.LoanAgent.loan {
        agent.ThreadPool.startAndGetHalter(runnable)
      }
      assert(success)
    }

    checkList should be (
      List("enter", "runOnce", "runOnce", "runOnce", "exit"))
  }

  "ThreadPool" should "実装された後処理は例外が起きても実行" in {
    var checkList = List[String]()

    val runnable = new agent.ThreadPool.Runnable() {
      override def enter() { checkList :+= "enter" }

      override def runOnce() = {
        Thread.sleep(wait_ms)
        checkList :+= "runOnce"
        // 三回のみ実行
        if (nextLoopCount < 3) {
          true
        } else {
          throw new Exception("this is exception for test.")
        }
      }

      override def exit() { checkList :+= "exit" }
    }

    Atmosphere.withTestIOUtils {
      val success = agent.LoanAgent.loan {
        agent.ThreadPool.startAndGetHalter(runnable)
      }
      assert(success)
    }

    checkList should be (
      List("enter", "runOnce", "runOnce", "runOnce", "exit"))
  }

  "ThreadPool" should "全てのスレッドが実行完了してから終了処理" in {
    var checkSet = Set[String]()

    val shortRunnable = new agent.ThreadPool.Runnable() {
      override def runOnce() = {
        Thread.sleep(wait_ms)
        nextLoopCount < 5
      }

      override def exit() {
        agent.ThreadPool.synchronized { checkSet += "exitShort" }
      }
    }

    val middleRunnable = new agent.ThreadPool.Runnable() {
      override def runOnce() = {
        Thread.sleep(wait_ms)
        nextLoopCount < 10
      }

      override def exit() {
        agent.ThreadPool.synchronized { checkSet += "exitMiddle" }
      }
    }

    val longRunnable = new agent.ThreadPool.Runnable() {
      override def runOnce() = {
        Thread.sleep(wait_ms)
        nextLoopCount < 15
      }

      override def exit() {
        agent.ThreadPool.synchronized { checkSet += "exitLong" }
      }
    }

    Atmosphere.withTestIOUtils {
      val startTime_ns = System.nanoTime()
      val success = agent.LoanAgent.loan {
        assert(agent.ThreadPool.size === 0)
        agent.ThreadPool.startAndGetHalter(shortRunnable)
        agent.ThreadPool.startAndGetHalter(middleRunnable)
        agent.ThreadPool.startAndGetHalter(longRunnable)
        assert(agent.ThreadPool.size === 3)
      }
      assert(success)
      val endTime_ns = System.nanoTime()
      assert(endTime_ns - startTime_ns > wait_ns * 15)
      assert(endTime_ns - startTime_ns < wait_ns * 20)
      checkSet should be (Set("exitShort", "exitMiddle", "exitLong"))
    }
  }

  "ThreadPool" should "例外発生スレッドがあれば即時終了処理" in {
    var checkSet = Set[String]()

    val shortRunnable = new agent.ThreadPool.Runnable() {
      override def runOnce() = {
        Thread.sleep(wait_ms)
        // 例外により終了
        throw new Exception("this is exception for test.")
      }

      override def exit() {
        agent.ThreadPool.synchronized { checkSet += "exitShort" }
      }
    }

    val middleRunnable = new agent.ThreadPool.Runnable() {
      override def runOnce() = {
        Thread.sleep(wait_ms)
        nextLoopCount < 50
      }

      override def exit() {
        agent.ThreadPool.synchronized { checkSet += "exitMiddle" }
        // 終了処理で例外が発生
        throw new Exception("this is second exception for test.")
      }
    }

    val longRunnable = new agent.ThreadPool.Runnable() {
      override def runOnce() = {
        Thread.sleep(wait_ms)
        nextLoopCount < 100
      }

      override def exit() {
        agent.ThreadPool.synchronized { checkSet += "exitLong" }
      }
    }

    val testIOUtils = Atmosphere.withTestIOUtils {
      val startTime_ns = System.nanoTime()
      val success = agent.LoanAgent.loan {
        assert(agent.ThreadPool.size === 0)
        agent.ThreadPool.startAndGetHalter(shortRunnable)
        agent.ThreadPool.startAndGetHalter(middleRunnable)
        agent.ThreadPool.startAndGetHalter(longRunnable)
        assert(agent.ThreadPool.size === 3)
      }
      assert(success)
      val endTime_ns = System.nanoTime()
      assert(endTime_ns - startTime_ns < wait_ns * 10)
      checkSet should be (Set("exitShort", "exitMiddle", "exitLong"))
    }
    val error = testIOUtils.dumpStdErr()
    error should include (
      "FATAL: java.lang.Exception: this is exception for test.")
    error should include (
      "ERROR: java.lang.Exception: this is second exception for test.")
  }

  "ThreadPool" should "呼び出しスレッドの例外終了で即時終了処理" in {
    var checkSet = Set[String]()

    val infiniteRunnable = new agent.ThreadPool.Runnable() {
      override def runOnce() = {
        Thread.sleep(wait_ms)
        true
      }

      override def exit() {
        agent.ThreadPool.synchronized { checkSet += "exitInfinite" }
      }
    }

    val testIOUtils = Atmosphere.withTestIOUtils {
      val startTime_ns = System.nanoTime()
      val success = agent.LoanAgent.loan {
        assert(agent.ThreadPool.size === 0)
        agent.ThreadPool.startAndGetHalter(infiniteRunnable)
        assert(agent.ThreadPool.size === 1)
        // 例外により終了
        throw new Exception("this is exception for test.")
      }
      assert(!success)
      val endTime_ns = System.nanoTime()
      assert(endTime_ns - startTime_ns < wait_ns * 5)
      checkSet should be (Set("exitInfinite"))
    }
    val error = testIOUtils.dumpStdErr()
    error should include (
      "FATAL: java.lang.Exception: ゲーム実行中にエラーが発生しました。")
    error should include (
      "FATAL: Caused by: java.lang.Exception: this is exception for test.")
  }

  "ThreadPool" should "規定値以上のスレッドが起動されると警告ログが出る" in {
    def getRunnable() = new agent.ThreadPool.Runnable() {
      override def runOnce() = {
        Thread.sleep(wait_ms)
        true
      }
    }

    {
      val testIOUtils = Atmosphere.withTestIOUtils {
        val success = agent.LoanAgent.loan {
          assert(agent.ThreadPool.size === 0)
          // ちょうど規定値以内のスレッドを起動
          (0 until agent.ThreadPool.warnThreadCount).foreach { case _ =>
            agent.ThreadPool.startAndGetHalter(getRunnable())
          }
          assert(agent.ThreadPool.size === agent.ThreadPool.warnThreadCount)
          // 例外により強制終了
          throw new Exception("this is exception for test.")
        }
        assert(!success)
      }
      val error = testIOUtils.dumpStdErr()
      error should include (
        "FATAL: java.lang.Exception: ゲーム実行中にエラーが発生しました。")
      error should include (
        "FATAL: Caused by: java.lang.Exception: this is exception for test.")
      error should not include ("WARN: Too many threads: ")
    }

    {
      val testIOUtils = Atmosphere.withTestIOUtils {
        val success = agent.LoanAgent.loan {
          assert(agent.ThreadPool.size === 0)
          // ちょうど規定値を超えるスレッドを起動
          (0 until agent.ThreadPool.warnThreadCount + 1).foreach { case _ =>
            agent.ThreadPool.startAndGetHalter(getRunnable())
          }
          assert(agent.ThreadPool.size === agent.ThreadPool.warnThreadCount + 1)
          // 例外により強制終了
          throw new Exception("this is exception for test.")
        }
        assert(!success)
      }
      val error = testIOUtils.dumpStdErr()
      error should include (
        "FATAL: java.lang.Exception: ゲーム実行中にエラーが発生しました。")
      error should include (
        "FATAL: Caused by: java.lang.Exception: this is exception for test.")
      error should include ("WARN: Too many threads: ")
    }
  }

  "ThreadPool" should "スレッドプール起動前にスレッドを利用すると例外" in {
    // 他テストによるスレッドプール起動を避けるためにロック取得
    agent.LoanAgent.synchronized {
      intercept[agent.ThreadPool.IsNotReady] {
        agent.ThreadPool.startAndGetHalter(new agent.ThreadPool.Runnable() {
          override def runOnce() = {
            Thread.sleep(wait_ms)
            true
          }
        })
      }
    }
  }

  "IntervalRunnable" should "定期的にintervalRunOnceの内容を実行" in {
    var count = 0

    val runnable = new agent.ThreadPool.IntervalRunnable() {
      override val fps = 1000 / wait_ms
      override def intervalRunOnce() = {
        count += 1
        true
      }
    }

    Atmosphere.withTestIOUtils {
      val success = agent.LoanAgent.loan {
        val halter = agent.ThreadPool.startAndGetHalter(runnable)
        Thread.sleep(wait_ms / 2)
        for (expectedCount <- 1 to 10) {
          assert(count === expectedCount)
          Thread.sleep(wait_ms)
        }
        halter()
      }
      assert(success)
    }
  }

  "IntervalRunnable" should "intervalRunOnceに時間がかかるなら連続実行" in {
    var count = 0

    val runnable = new agent.ThreadPool.IntervalRunnable() {
      override val fps = 1000 / wait_ms
      override def intervalRunOnce() = {
        count += 1
        Thread.sleep(wait_ms * 2)
        true
      }
    }

    Atmosphere.withTestIOUtils {
      val success = agent.LoanAgent.loan {
        val halter = agent.ThreadPool.startAndGetHalter(runnable)
        Thread.sleep(wait_ms)
        for (expectedCount <- 1 to 10) {
          assert(count === expectedCount)
          Thread.sleep(wait_ms * 2)
        }
        halter()
      }
      assert(success)
    }
  }
}

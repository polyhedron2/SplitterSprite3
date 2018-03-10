import org.scalatest.{FlatSpec, DiagrammedAssertions}

import jp.gr.java_conf.polyhedron.splittersprite3.agent

class ThreadPoolSpec extends FlatSpec with DiagrammedAssertions {
  val wait_ms = 10

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

    agent.ThreadPool.loan {
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

    agent.ThreadPool.loan {
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
  }
}

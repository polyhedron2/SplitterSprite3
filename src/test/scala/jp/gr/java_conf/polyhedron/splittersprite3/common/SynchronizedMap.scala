import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}

import jp.gr.java_conf.polyhedron.splittersprite3.agent
import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.common

class SynchronizedMapSpec
    extends FlatSpec with DiagrammedAssertions with Matchers {

  "SynchronizedMap" should "マルチスレッドで更新してもすべての値を保持" in {
    val map = new common.SynchronizedMap[String, Int]()

    val runnableX = new agent.ThreadPool.Runnable() {
      override def runOnce() = {
        map(f"x${loopCount}%02d") = loopCount
        // ループ100回で停止
        nextLoopCount < 100
      }
    }

    val runnableY = new agent.ThreadPool.Runnable() {
      override def runOnce() = {
        map(f"y${loopCount}%02d") = loopCount
        // ループ100回で停止
        nextLoopCount < 100
      }
    }

    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        agent.ThreadPool.startAndGetHalter(runnableX)
        agent.ThreadPool.startAndGetHalter(runnableY)
      }
      map.keys.toSet should be (
        (0 until 100).map(n => f"x${n}%02d").toSet ++
        (0 until 100).map(n => f"y${n}%02d").toSet)
      for (i <- 0 until 100) {
        assert(map(f"x${i}%02d") === i)
        assert(map(f"y${i}%02d") === i)
      }
    }
  }
}

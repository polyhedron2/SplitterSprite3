package jp.gr.java_conf.polyhedron.splittersprite3.agent

import java.lang.{Thread => JThread}

import jp.gr.java_conf.polyhedron.splittersprite3.common

object ThreadPool extends LoanAgent {
  private var runningThreads = Set[Thread]()

  //スレッドオブジェクトを２回スタートしないために開始後に終了用関数のみ返す
  def startAndGetHalter(runnable: common.Runnable) {
    synchronized {
      val thread = new Thread(runnable)
      add(thread)
      thread.start()
      thread.halt
    }
  }

  private def add(thread: Thread) = synchronized {
    runningThreads += thread
    if (runningThreads.size > 100) {
      Logger.warnLog(s"Thread count is ${runningThreads.size}")
    }
  }

  private def remove(thread: Thread) = synchronized {
    runningThreads -= thread
  }

  override def exit(exOpt: Option[Exception]) {
    runningThreads.foreach { _.halt() }
    exOpt.foreach { throw _ }
  }

  private class Thread(runnable: common.Runnable) extends JThread(runnable) {
    override def run() {
      super.run()
      remove(this)
    }

    def halt() { runnable.halt() }
  }
}

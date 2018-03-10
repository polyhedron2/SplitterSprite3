package jp.gr.java_conf.polyhedron.splittersprite3.agent

import java.lang.{Runnable => JRunnable}
import java.lang.{Thread => JThread}

import jp.gr.java_conf.polyhedron.splittersprite3.common

object ThreadPool extends LoanAgent {
  private var runningThreads = Set[Thread]()

  //スレッドオブジェクトを２回スタートしないために開始後に終了用関数のみ返す
  def startAndGetHalter(runnable: Runnable) {
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

  private class Thread(runnable: Runnable) extends JThread(runnable) {
    override def run() {
      super.run()
      remove(this)
    }

    def halt() { runnable.halt() }
  }

  //スレッド実行対象トレイト
  trait Runnable extends JRunnable {
    private var done = false
    //停止用メソッド
    def halt(): Unit = synchronized { done = true }

    override def run() {
      val exOpt = try {
        enter()
        while (!done && runOnce()) { }
        None
      } catch {
        case e: Exception => Some(e)
      }
      exit(exOpt)
    }

    //処理開始時に一度だけ呼ばれるメソッド
    def enter() { }
    //処理終了時に一度だけ呼ばれるメソッド, 例外があっても実行される
    def exit(exOpt: Option[Exception]): Unit = exOpt.foreach { throw _ }
    //ループされる処理　戻り値はループを継続するか否か
    def runOnce(): Boolean
  }
}

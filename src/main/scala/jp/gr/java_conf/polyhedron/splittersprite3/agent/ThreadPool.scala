package jp.gr.java_conf.polyhedron.splittersprite3.agent

import java.lang.{Runnable => JRunnable}
import java.lang.{Thread => JThread}

import jp.gr.java_conf.polyhedron.splittersprite3.common

object ThreadPool extends LoanAgent {
  class IsNotReady extends Exception("スレッドプールが起動されていません。")

  val waitLoop_ms = 100
  private var runningThreads = Set[Thread]()
  private var uncaughtExceptions = List[Exception]()

  //スレッドオブジェクトを２回スタートしないために開始後に終了用関数のみ返す
  def startAndGetHalter(runnable: Runnable): () => Unit = {
    if (!alreadyUsing) { throw new IsNotReady() }

    val thread = new Thread(runnable)
    synchronized { runningThreads += thread }
    val _size = size
    if (_size > 100) { Logger.warnLog(s"Too many threads: ${_size}") }

    thread.start()
    thread.halt
  }

  def size: Int = synchronized { runningThreads.size }

  override def exit(exOpt: Option[Exception]) {
    def finish =
      synchronized { runningThreads.isEmpty } ||
      synchronized { !uncaughtExceptions.isEmpty }

    // 他スレッドが全て完了するかで例外が生じるまで呼び出し元スレッドを停止
    while (!finish) { Thread.sleep(waitLoop_ms) }

    // 例外発生時の場合のために全スレッドに停止指示
    synchronized { runningThreads.foreach { _.halt() } }

    // 全スレッドの完了待ち
    while (synchronized { !runningThreads.isEmpty }) {
      Thread.sleep(waitLoop_ms)
    }

    // 例外発生時にはそれをログに出力
    synchronized { uncaughtExceptions.foreach {
      Logger.printStackTrace(_, Logger.Fatal) } }

    // Loan中の自スレッドの例外はそのままthrow
    exOpt.foreach { throw _ }
  }

  private class Thread(runnable: Runnable) extends JThread(runnable) {
    override def run() {
      super.run()
      ThreadPool.synchronized { runningThreads -= this }
    }

    def halt() { runnable.halt() }
  }

  //スレッド実行対象トレイト
  trait Runnable extends JRunnable {
    private var done = false
    //停止用メソッド
    def halt() { done = true }

    override def run() {
      try {
        enter()
        while (!done && runOnce()) { }
      } catch {
        case e: Exception => synchronized { uncaughtExceptions :+= e }
      } finally {
        exit()
      }
    }

    //処理開始時に一度だけ呼ばれるメソッド
    def enter() { }
    //処理終了時に一度だけ呼ばれるメソッド, 例外があっても実行される
    def exit() { }
    //ループされる処理　戻り値はループを継続するか否か
    def runOnce(): Boolean
  }
}

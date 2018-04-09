package jp.gr.java_conf.polyhedron.splittersprite3.agent

import java.lang.{Runnable => JRunnable}
import java.lang.{Thread => JThread}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.common

object ThreadPool extends LoanAgent {
  class IsNotReady extends Exception("スレッドプールが起動されていません。")

  val waitLoop_ms = 100
  val warnThreadCount = 100
  private var runningThreads = Set[Thread]()
  private var uncaughtExceptions = List[Exception]()

  //スレッドオブジェクトを２回スタートしないために開始後に終了用関数のみ返す
  def startAndGetHalter(runnable: Runnable): () => Unit = {
    if (!alreadyUsing) { throw new IsNotReady() }

    val thread = new Thread(runnable)
    synchronized { runningThreads += thread }
    val _size = size
    if (_size > warnThreadCount) {
      Logger.warnLog(s"Too many threads: ${_size} (> ${warnThreadCount})")
    }

    thread.start()
    () => thread.halt()
  }

  def size: Int = synchronized { runningThreads.size }

  override def exit(exOpt: Option[Exception]) {
    def finish =
      synchronized { runningThreads.isEmpty } ||
      synchronized { !uncaughtExceptions.isEmpty }

    Logger.infoLog("Waiting game end...")

    exOpt match {
      // 元スレッドに例外があった場合には即時終了処理
      case Some(ex) =>
      // 他スレッドが全て完了するかで例外が生じるまで呼び出し元スレッドを停止
      case None => while (!finish) { Thread.sleep(waitLoop_ms) }
    }

    Logger.infoLog("Closing ThreadPool...")

    // 別スレッド例外による終了であれば原因をFatal出力
    synchronized {
      if (!uncaughtExceptions.isEmpty) {
        Logger.fatalLog("Game is stopped because of sub thread exception.")
      }
      uncaughtExceptions.foreach { Logger.printStackTrace(_, Logger.Fatal) }
      uncaughtExceptions = List()
    }

    // 全スレッドに停止指示
    Logger.infoLog("Halting all sub threads...")
    synchronized { runningThreads.foreach { _.halt() } }

    Logger.infoLog("Waiting all sub threads...")
    // 全スレッドの完了待ち
    while (synchronized { !runningThreads.isEmpty }) {
      Thread.sleep(waitLoop_ms)
    }

    // 完了処理中の例外発生はError出力
    synchronized {
      if (!uncaughtExceptions.isEmpty) {
        Logger.errorLog("Following errors happend in halt.")
      }
      uncaughtExceptions.foreach { Logger.printStackTrace(_, Logger.Error) }
    }

    Logger.infoLog("ThreadPool is successfully closed.")
    // Loan中の自スレッドの例外はそのままthrow
    exOpt.foreach { throw _ }
  }

  private class Thread(runnable: Runnable) extends JThread(runnable) {
    override def run() {
      try {
        runnable.enter()
        super.run()
      } catch {
        case e: Exception =>
          ThreadPool.synchronized { uncaughtExceptions :+= e }
      } finally {
        try {
          runnable.exit()
        } catch {
          case e: Exception =>
            ThreadPool.synchronized { uncaughtExceptions :+= e }
        } finally {
          ThreadPool.synchronized { runningThreads -= this }
        }
      }
    }

    def halt() { runnable.halt() }
  }

  //スレッド実行対象トレイト
  trait Runnable extends JRunnable {
    private var innerLoopCount = 0
    def loopCount: Int = innerLoopCount
    def nextLoopCount: Int = loopCount + 1

    private var done = false
    //停止用メソッド
    def halt() { done = true }

    override def run() {
      while (!done && runOnce()) { innerLoopCount += 1 }
    }

    //処理開始時に一度だけ呼ばれるメソッド
    def enter() { }
    //処理終了時に一度だけ呼ばれるメソッド, 例外があっても実行される
    def exit() { }
    //ループされる処理　戻り値はループを継続するか否か
    def runOnce(): Boolean
  }

  trait IntervalRunnable extends Runnable {
    var beforeTime_ms = 0L

    override def enter() {
      beforeTime_ms = Atmosphere.timeUtils.currentTimeMillis
    }

    override def runOnce(): Boolean = {
      val continue = intervalRunOnce()
      val afterTime_ms = Atmosphere.timeUtils.currentTimeMillis
      val elapsedTime_ms = afterTime_ms - beforeTime_ms
      val sleepTime_ms = (intervalTime_ms - elapsedTime_ms) max 0
      Thread.sleep(sleepTime_ms)
      beforeTime_ms = Atmosphere.timeUtils.currentTimeMillis
      continue
    }

    def intervalTime_ms = 1000 / fps

    //インターバル付きでループされる処理　戻り値はループを継続するか否か
    def intervalRunOnce(): Boolean
    def fps: Int
  }
}

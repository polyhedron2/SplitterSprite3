package jp.gr.java_conf.polyhedron.splittersprite3.common

import java.lang.{Runnable => JRunnable}

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


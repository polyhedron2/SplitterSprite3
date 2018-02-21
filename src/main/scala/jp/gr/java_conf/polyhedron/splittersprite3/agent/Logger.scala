package jp.gr.java_conf.polyhedron.splittersprite3.agent

import java.io.{PrintStream, PrintWriter}
import java.nio.file.{Files, Paths, Path => JPath, FileSystems}
import java.util.{Calendar}

import jp.gr.java_conf.polyhedron.splittersprite3.common

sealed abstract class LogLevel {
  val level: Int
  val name: String
  def <(that: LogLevel) = this.level < that.level
  def <=(that: LogLevel) = this.level <= that.level
  def >(that: LogLevel) = this.level > that.level
  def >=(that: LogLevel) = this.level >= that.level
}
// 致命的エラー　プログラムの異常終了など
case object Fatal extends LogLevel { val level = 5; val name = "FATAL" }
// 予期せぬ実行時エラー
case object Error extends LogLevel { val level = 4; val name = "ERROR" }
// 警告　不適切なAPI使用、予期せぬ状態
case object Warn extends LogLevel { val level = 3; val name = "WARN" }
// 情報　実行時の注目すべき事象、処理の開始・終了など
case object Info extends LogLevel { val level = 2; val name = "INFO" }
// デバッグ情報　システムの動作状況
case object Debug extends LogLevel { val level = 1; val name = "DEBUG" }
// トレース情報　システムの詳細な動作状況
case object Trace extends LogLevel { val level = 0; val name = "TRACE" }

// ログ出力管理を行うシングルトン
object Logger {
  var logLevel: LogLevel = Info

  val logPath = {
    val cal = Calendar.getInstance()
    val localLogPath = Paths.get(
      "log",
      f"${cal.get(Calendar.YEAR)}%04d_" +
      f"${cal.get(Calendar.MONTH) + 1}%02d_" +
      f"${cal.get(Calendar.DATE)}%02d_" +
      f"${cal.get(Calendar.HOUR_OF_DAY)}%02d_" +
      f"${cal.get(Calendar.MINUTE)}%02d_" +
      f"${cal.get(Calendar.SECOND)}%02d_" +
      f"${cal.get(Calendar.MILLISECOND)}%03d" +
      s".log")
    common.FileUtils.gameDirPath.resolve(localLogPath)
  }

  private val stderr = new PrintStream(System.err, true, "UTF-8")
  private val writer = new PrintWriter(Files.newBufferedWriter(logPath), true)

  private def println(message: String) = synchronized {
    stderr.println(message)
    writer.println(message)
  }

  private def showMessage(messageLevel: LogLevel, message: String) {
    if (messageLevel >= logLevel) {
      println(s"${messageLevel.name}: ${message}")
    }
  }

  def fatalLog(message: String) = showMessage(Fatal, message)
  def errorLog(message: String) = showMessage(Error, message)
  def warnLog(message: String) = showMessage(Warn, message)
  def infoLog(message: String) = showMessage(Info, message)
  def debugLog(message: String) = showMessage(Debug, message)
  def traceLog(message: String) = showMessage(Trace, message)

  def close() = writer.close()
}

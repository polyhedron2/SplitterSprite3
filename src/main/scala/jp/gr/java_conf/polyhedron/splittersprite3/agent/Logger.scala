package jp.gr.java_conf.polyhedron.splittersprite3.agent

import java.io.{PrintStream, PrintWriter, StringWriter}
import java.nio.file.{Files, Paths, Path => JPath, FileSystems}
import java.util.{Calendar}

import jp.gr.java_conf.polyhedron.splittersprite3.Atmosphere

// ログ出力管理を行うシングルトン
object Logger extends LoanAgent {
  var logLevel: LogLevel = Info

  private val stderr = new PrintStream(System.err, true, "UTF-8")
  private var writerOpt: Option[PrintWriter] = None

  private def buildWriter() = {
    val logPath = {
      val cal = Calendar.getInstance()
      val localLogPath = Paths.get(
        "log",
        f"${cal.get(Calendar.YEAR)}%04d-" +
        f"${cal.get(Calendar.MONTH) + 1}%02d-" +
        f"${cal.get(Calendar.DATE)}%02d_" +
        f"${cal.get(Calendar.HOUR_OF_DAY)}%02d:" +
        f"${cal.get(Calendar.MINUTE)}%02d:" +
        f"${cal.get(Calendar.SECOND)}%02d." +
        f"${cal.get(Calendar.MILLISECOND)}%03d" +
        s".log")
      Atmosphere.gameDirPath.resolve(localLogPath)
    }

    new PrintWriter(Files.newBufferedWriter(logPath), true)
  }

  private def println(message: String) = synchronized {
    stderr.println(message)
    writerOpt match {
      case Some(writer) => writer.println(message)
      case None => stderr.println("StackTrace is not logged into log file.")
    }
  }

  private def showMessage(messageLevel: LogLevel, message: String) {
    if (messageLevel >= logLevel) {
      println(s"${messageLevel.name}: ${message}")
    }
  }

  def fatalLog(message: String) { showMessage(Fatal, message) }
  def errorLog(message: String) { showMessage(Error, message) }
  def warnLog(message: String) { showMessage(Warn, message) }
  def infoLog(message: String) { showMessage(Info, message) }
  def debugLog(message: String) { showMessage(Debug, message) }
  def traceLog(message: String) { showMessage(Trace, message) }

  def showPropertyInfo(key: String) {
    infoLog(s"${key}: ${System.getProperty(key)}")
  }

  def stackTraceLines(ex: Exception): Iterator[String] = {
    val stringWriter = new StringWriter()
    val printWriter = new PrintWriter(stringWriter, true)
    ex.printStackTrace(printWriter)
    val ret = stringWriter.toString().lines
    printWriter.close()
    ret
  }

  private def printStackTrace(ex: Exception) {
    try {
      ex.printStackTrace(stderr)
      writerOpt match {
        case Some(writer) => ex.printStackTrace(writer)
        case None => stderr.println("StackTrace is not logged into log file.")
      }
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

  def printStackTrace(ex: Exception, messageLevel: LogLevel) {
    try {
      stackTraceLines(ex).foreach(showMessage(messageLevel, _))
    } catch {
      case e: Exception => {
        printStackTrace(ex)
        printStackTrace(e)
      }
    }
  }

  override def enter() {
    writerOpt = Some(buildWriter())
    infoLog("================== SYSTEM PROPERTY ==================")
    showPropertyInfo("java.version")
    showPropertyInfo("java.runtime.version")
    showPropertyInfo("java.runtime.name")
    showPropertyInfo("java.vm.version")
    showPropertyInfo("java.vm.name")
    infoLog("=====================================================")
  }

  override def exit(exOpt: Option[Exception]) {
    exOpt.foreach { case ex => printStackTrace(
      new Exception("ゲーム実行中にエラーが発生しました。", ex), Fatal) }
    infoLog("Closing...")
    writerOpt.foreach(_.close())
  }

  sealed abstract class LogLevel {
    val level: Int
    val name: String
    def <(that: LogLevel): Boolean = this.level < that.level
    def <=(that: LogLevel): Boolean = this.level <= that.level
    def >(that: LogLevel): Boolean = this.level > that.level
    def >=(that: LogLevel): Boolean = this.level >= that.level
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
}

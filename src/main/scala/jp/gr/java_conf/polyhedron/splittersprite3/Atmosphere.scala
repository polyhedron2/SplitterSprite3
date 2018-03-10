package jp.gr.java_conf.polyhedron.splittersprite3

import java.io.{Reader, PrintStream, ByteArrayOutputStream}
import java.nio.charset.{StandardCharsets}
import java.nio.file.{Path}

// ゲームのすべての入出力（プレイヤーの操作、ファイル読み書き、乱数、時刻）
// を管理するシングルトン
// 内部パスの実体パスへの変換を行う。
object Atmosphere {
  private val defaultGameDirPath = outerspace.FileUtils.gameDirPath
  private var innerGameDirPath = defaultGameDirPath
  def gameDirPath: Path = innerGameDirPath

  private val defaultStdErrStream = new PrintStream(System.err, true, "UTF-8")
  private var innerStdErrStream = defaultStdErrStream
  def stdErrStream: PrintStream = innerStdErrStream

  def withMockedGameDirPath(path: Path)(op: => Any) {
    val before = innerGameDirPath
    try {
      innerGameDirPath = path
      op
    } finally {
      innerGameDirPath = before
    }
  }

  def withTmpGameDirPath(op: => Any) {
    withMockedGameDirPath(outerspace.FileUtils.tmpDirPath)(op)
  }

  // 戻り値は標準エラー出力への内容
  def withMockedStdErrStream(op: => Any): String = {
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos, true, "utf-8")
    val before = innerStdErrStream
    try {
      innerStdErrStream = ps
      op
    } finally {
      innerStdErrStream = before
    }
    new String(baos.toByteArray(), StandardCharsets.UTF_8)
  }

  def withReader[T](patchablePath: String)(op: Reader => T): T = {
    val reader = outerspace.FileUtils.patchedReader(patchablePath)
    try { op(reader) } finally { reader.close() }
  }
}

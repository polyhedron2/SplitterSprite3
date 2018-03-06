package jp.gr.java_conf.polyhedron.splittersprite3

import java.io.{Reader}

// ゲームのすべての入出力（プレイヤーの操作、ファイル読み書き、乱数、時刻）
// を管理するシングルトン
// 内部パスの実体パスへの変換を行う。
object Atmosphere {
  val gameDirPath = outerspace.FileUtils.gameDirPath

  def withReader[T](patchablePath: String)(op: Reader => T): T = {
    val reader = outerspace.FileUtils.patchedReader(patchablePath)
    try { op(reader) } finally { reader.close() }
  }
}

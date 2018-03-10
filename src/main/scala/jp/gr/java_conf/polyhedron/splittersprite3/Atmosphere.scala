package jp.gr.java_conf.polyhedron.splittersprite3

import java.io.{Reader}
import java.nio.file.{Path, Paths}

// ゲームのすべての入出力（プレイヤーの操作、ファイル読み書き、乱数、時刻）
// を管理するシングルトン
// 内部パスの実体パスへの変換を行う。
object Atmosphere {
  private val defaultIOUtils: outerspace.IOUtils =
    new outerspace.ProductionIOUtils()
  private var innerIOUtils = defaultIOUtils
  def ioUtils: outerspace.IOUtils = innerIOUtils

  // TestIOUtilsを用いて処理を実行し、戻り値として返す
  def withTestIOUtils(gameJarPathFromGameDirPath: Path)(op: => Any):
      outerspace.TestIOUtils = {
    val prevIOUtils = innerIOUtils
    val testIOUtils = new outerspace.TestIOUtils(gameJarPathFromGameDirPath)
    try {
      innerIOUtils = testIOUtils
      op
    } finally {
      innerIOUtils = prevIOUtils
    }
    testIOUtils
  }

  def withTestIOUtils(op: => Any): outerspace.TestIOUtils =
    withTestIOUtils(Paths.get("ver1.0.0", "game.jar"))(op)
}

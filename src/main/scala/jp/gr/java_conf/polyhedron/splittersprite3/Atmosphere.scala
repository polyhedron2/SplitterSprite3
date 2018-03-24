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

  private val defaultTimeUtils: outerspace.TimeUtils =
    new outerspace.ProductionTimeUtils()
  private var innerTimeUtils = defaultTimeUtils
  def timeUtils: outerspace.TimeUtils = innerTimeUtils

  // TestIOUtilsを用いて処理を実行し、戻り値として返す
  def withTestIOUtils(
    gameDirPathFromTestDirPath: Path,
    gameJarPathFromGameDirPath: Path)(op: => Any):
      outerspace.TestIOUtils = synchronized {
    val prevIOUtils = innerIOUtils
    val testIOUtils = new outerspace.TestIOUtils(
      gameDirPathFromTestDirPath, gameJarPathFromGameDirPath)
    try {
      innerIOUtils = testIOUtils
      testIOUtils.enter()
      op
    } finally {
      testIOUtils.exit()
      innerIOUtils = prevIOUtils
    }
    testIOUtils
  }

  def withTestIOUtils(
      gameJarPathFromGameDirPath: Path)(op: => Any): outerspace.TestIOUtils =
    withTestIOUtils(Paths.get(""), gameJarPathFromGameDirPath)(op)

  def withTestIOUtils(op: => Any): outerspace.TestIOUtils =
    withTestIOUtils(Paths.get("ver1.0.0", "game.jar"))(op)

  def withTestTimeUtils(
      currentTimeMillisList: List[Long], timeZoneID: String)(op: => Any):
      outerspace.TestTimeUtils = {
    val prevTimeUtils = innerTimeUtils
    val testTimeUtils = new outerspace.TestTimeUtils(
      currentTimeMillisList, timeZoneID)
    try {
      innerTimeUtils = testTimeUtils
      op
    } finally {
      innerTimeUtils = prevTimeUtils
      if (!testTimeUtils.currentTimeMillisList.isEmpty) {
        throw new outerspace.TestTimeUtils.TooManyCurrentMillisException()
      }
    }
    testTimeUtils
  }

  def withTestTimeUtils(
      currentTimeMillisList: List[Long])(op: => Any):
      outerspace.TestTimeUtils =
    withTestTimeUtils(currentTimeMillisList, "Asia/Tokyo")(op)
}

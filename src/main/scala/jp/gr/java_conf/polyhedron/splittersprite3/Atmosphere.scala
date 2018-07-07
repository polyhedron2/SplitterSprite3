package jp.gr.java_conf.polyhedron.splittersprite3

import java.io.{Reader}
import java.nio.file.{Path => NioPath, Paths => NioPaths}
import javafx.scene.canvas.{GraphicsContext}
import javafx.stage.{Stage}

// ゲームのすべての入出力（プレイヤーの操作、ファイル読み書き、乱数、時刻）
// を管理するシングルトン
// 内部パスの実体パスへの変換を行う。
object Atmosphere {
  private var innerIOUtils: outerspace.IOUtils =
    new outerspace.ProductionIOUtils()
  def ioUtils: outerspace.IOUtils = innerIOUtils

  private var innerTimeUtils: outerspace.TimeUtils =
    new outerspace.ProductionTimeUtils()
  def timeUtils: outerspace.TimeUtils = innerTimeUtils

  private var innerReflectionUtils: outerspace.ReflectionUtils =
    new outerspace.ProductionReflectionUtils()
  def reflectionUtils: outerspace.ReflectionUtils = innerReflectionUtils

  // ウィンドウごとの名前をキー、CommandRegulatorをバリューとするMap
  val commandRegulator =
    new common.Cache[String, outerspace.CommandRegulator] {
      override def calc(windowName: String) =
        new outerspace.CommandRegulator()
    }

  val javaFXTaskQueue =
    new scala.collection.mutable.SynchronizedQueue[Stage => Any]()

  // TestIOUtilsを用いて処理を実行し、戻り値として返す
  def withTestIOUtils(
    gameDirPathFromTestDirPath: NioPath,
    gameJarPathFromGameDirPath: NioPath)(op: => Any):
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
      gameJarPathFromGameDirPath: NioPath)(op: => Any): outerspace.TestIOUtils =
    withTestIOUtils(NioPaths.get(""), gameJarPathFromGameDirPath)(op)

  def withTestIOUtils(op: => Any): outerspace.TestIOUtils =
    withTestIOUtils(NioPaths.get("ver1.0.0", "game.jar"))(op)

  def withTestTimeUtils(
      currentTimeMillisList: List[Long], timeZoneID: String)(op: => Any):
      outerspace.TestTimeUtils = synchronized {
    val prevTimeUtils = innerTimeUtils
    val testTimeUtils = new outerspace.TestTimeUtils(
      currentTimeMillisList, timeZoneID)
    try {
      innerTimeUtils = testTimeUtils
      op
      if (!testTimeUtils.currentTimeMillisList.isEmpty) {
        throw new outerspace.TestTimeUtils.TooManyCurrentTimeMillisException()
      }
    } finally {
      innerTimeUtils = prevTimeUtils
    }
    testTimeUtils
  }

  def withTestTimeUtils(
      currentTimeMillisList: List[Long])(op: => Any):
      outerspace.TestTimeUtils =
    withTestTimeUtils(currentTimeMillisList, "Asia/Tokyo")(op)

  // TestReflectionUtilsを用いて処理を実行し、戻り値として返す
  def withTestReflectionUtils(classes: Class[_]*)(op: => Any):
      outerspace.TestReflectionUtils = synchronized {
    val classIterator = Iterator(classes:_*)
    val prevReflectionUtils = innerReflectionUtils
    val testReflectionUtils = new outerspace.TestReflectionUtils(classIterator)
    try {
      innerReflectionUtils = testReflectionUtils
      op
    } finally {
      innerReflectionUtils = prevReflectionUtils
    }
    testReflectionUtils
  }
}

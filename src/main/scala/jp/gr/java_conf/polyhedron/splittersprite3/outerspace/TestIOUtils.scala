package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import java.io.{PrintStream, ByteArrayOutputStream}
import java.nio.charset.{StandardCharsets}
import java.nio.file.{Files, Paths, Path => JPath}
import scala.collection.JavaConverters._

// gameJarPathFromGameDirPath: TmpDirから実行ファイルへの相対パス
class TestIOUtils(
    gameDirPathFromTestDirPath: JPath, gameJarPathFromGameDirPath: JPath)
    extends IOUtils {
  private val tmpDirPath =
    Paths.get(System.getProperty("java.io.tmpdir")).toAbsolutePath()
  private val testDirPath =
    Files.createTempDirectory(tmpDirPath, "splittersprite-test-ioutils-")
  private val tmpGameDirPath = testDirPath.resolve(gameDirPathFromTestDirPath)

  def enter() {
    Files.createDirectories(tmpGameDirPath)
  }

  def exit() {
    def recursiveDelete(path: JPath) {
      if (Files.isDirectory(path)) {
        val pathIter = Files.newDirectoryStream(path).iterator().asScala
        iterateDelete(pathIter)
      }
      Files.delete(path)
    }

    def iterateDelete(pathIter: Iterator[JPath]) {
      if (pathIter.hasNext) {
        recursiveDelete(pathIter.next)
        iterateDelete(pathIter)
      }
    }

    recursiveDelete(testDirPath)
  }

  val baos = new ByteArrayOutputStream()
  override val stderr = new PrintStream(baos, true, "utf-8")
  // 戻り値は標準エラー出力への内容
  def dumpStdErr(): String =
    new String(baos.toByteArray(), StandardCharsets.UTF_8)

  override val gameJarPath = tmpGameDirPath.resolve(gameJarPathFromGameDirPath)
}

package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import java.io.{PrintStream, ByteArrayOutputStream}
import java.nio.charset.{StandardCharsets}
import java.nio.file.{Files => NioFiles, Paths => NioPaths, Path => NioPath}
import scala.collection.JavaConverters._

// gameJarPathFromGameDirPath: TmpDirから実行ファイルへの相対パス
class TestIOUtils(
    gameDirPathFromTestDirPath: NioPath, gameJarPathFromGameDirPath: NioPath)
    extends IOUtils {
  private val tmpDirPath =
    NioPaths.get(System.getProperty("java.io.tmpdir")).toAbsolutePath()
  private val testDirPath =
    NioFiles.createTempDirectory(tmpDirPath, "splittersprite-test-ioutils-")
  private val tmpGameDirPath = testDirPath.resolve(gameDirPathFromTestDirPath)

  def enter() {
    NioFiles.createDirectories(tmpGameDirPath)
  }

  def exit() {
    def recursiveDelete(path: NioPath) {
      childrenList(path).foreach(recursiveDelete)
      NioFiles.delete(path)
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

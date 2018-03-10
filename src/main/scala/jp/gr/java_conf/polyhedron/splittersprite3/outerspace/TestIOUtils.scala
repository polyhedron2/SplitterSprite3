package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import java.io.{PrintStream, ByteArrayOutputStream}
import java.nio.charset.{StandardCharsets}
import java.nio.file.{Paths, Path => JPath}

// gameJarPathFromGameDirPath: TmpDirから実行ファイルへの相対パス
class TestIOUtils(gameJarPathFromGameDirPath: JPath) extends IOUtils {
  val tmpDirPath =
    Paths.get(System.getProperty("java.io.tmpdir")).toAbsolutePath()

  val baos = new ByteArrayOutputStream()
  override val stderr = new PrintStream(baos, true, "utf-8")
  // 戻り値は標準エラー出力への内容
  def dumpStdErr(): String =
    new String(baos.toByteArray(), StandardCharsets.UTF_8)

  override val gameJarPath = tmpDirPath.resolve(gameJarPathFromGameDirPath)
}

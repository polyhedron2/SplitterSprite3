package jp.gr.java_conf.polyhedron.splittersprite3.common

import java.io.{Reader}
import java.nio.file.{Files, Paths, Path => JPath, FileSystems}

class InvalidVersionName(name: String)
  extends Exception(s"ヴァージョン名として${name}は不正です。")
class InvalidVersionDirectory(jPath: JPath)
  extends Exception(s"ヴァージョンディレクトリとして${jPath}は不正です。")
class InvalidPatchDirectory(jPath: JPath)
  extends Exception(s"パッチディレクトリとして${jPath}は不正です。")
class InvaldPatchList(jPath: JPath)
  extends Exception(s"${jPath}へのパッチディレクトリの配置が不正です。")
class FileIsNotFound(internalPath: String)
  extends Exception(s"${internalPath}にファイルが存在しません。")

// ゲームのすべての入出力（プレイヤーの操作、ファイル読み書き、乱数、時刻）
// を管理するシングルトン
// 内部パスの実体パスへの変換を行う。
object Channel {
  // 実行ファイルのパス
  val gameJarPath =
    Paths.get(System.getProperty("java.class.path")).toAbsolutePath()
  val verOrPatchDirPath = gameJarPath.getParent()
  val gameDirPath = verOrPatchDirPath.getParent()
  val appliedPatchDirList = appliedPatchDirListTo(verOrPatchDirPath)

  def tailNameOf(jPath: JPath) =
    jPath.getName(jPath.getNameCount() - 1).toString

  // "verA.B.C"を(A, B, C)の整数３つ組にパース
  def parseVersionName(name: String) = {
    if (!name.startsWith("ver")) { throw new InvalidVersionName(name) }
    val splitNumberStrs = name.drop("ver".length).split('.')
    if (splitNumberStrs.length != 3) { throw new InvalidVersionName(name) }
    (splitNumberStrs(0), splitNumberStrs(1), splitNumberStrs(2))
  }

  // "verA.B.C"を(A, B, C)の整数３つ組にパース
  def parseVersionDirectory(jPath: JPath) = try {
    parseVersionName(tailNameOf(jPath))
  } catch {
    case e: InvalidVersionName => throw new InvalidVersionDirectory(jPath)
  }

  // "patch_from_verA.B.C_to_verX.Y.Z"を((A, B, C), (X, Y, Z))にパース
  def parsePatchDirectory(jPath: JPath) = try {
    val name = tailNameOf(jPath)
    if (!name.startsWith("patch_from_")) {
      throw new InvalidPatchDirectory(jPath)
    }
    val splitStrs = name.split('_')
    if (splitStrs.length != 5) { throw new InvalidPatchDirectory(jPath) }
    if (splitStrs(3) != "to") { throw new InvalidPatchDirectory(jPath) }
    (parseVersionName(splitStrs(2)), parseVersionName(splitStrs(4)))
  } catch {
    case e: InvalidVersionName => throw new InvalidPatchDirectory(jPath)
  }

  def isVersionDirectory(jPath: JPath) = Files.isDirectory(jPath) && (try {
    parseVersionDirectory(jPath)
    true
  } catch {
    case e: InvalidVersionDirectory => false
  })

  def isPatchDirectory(jPath: JPath) = Files.isDirectory(jPath) && (try {
    parsePatchDirectory(jPath)
    true
  } catch {
    case e: InvalidPatchDirectory => false
  })

  def appliedPatchDirListTo(jPath: JPath): List[JPath] =
    if (isVersionDirectory(jPath)) {
      return List(jPath)
    } else if (isPatchDirectory(jPath)) {
      val prevVersion = parsePatchDirectory(jPath)._1
      val dirIter = Files.newDirectoryStream(jPath).iterator()
      while (dirIter.hasNext()) {
        val dir = dirIter.next()
        if (isVersionDirectory(dir) &&
            parseVersionDirectory(dir) == prevVersion) {
          return jPath :: appliedPatchDirListTo(dir)
        } else if (isPatchDirectory(dir) &&
                   parsePatchDirectory(dir)._2 == prevVersion) {
          return jPath :: appliedPatchDirListTo(dir)
        }
      }
      throw new InvaldPatchList(jPath)
    } else {
      throw new InvaldPatchList(jPath)
    }

  def reader(internalPath: String): Reader = {
    val separator = FileSystems.getDefault().getSeparator()
    val internalJPath = Paths.get(internalPath.replace("/", separator))
    for (appliedPatchDir <- appliedPatchDirList) {
      val canonicalJPath = appliedPatchDir.resolve(internalJPath)
      if (Files.exists(canonicalJPath)) {
        // UTF-8として読み込み
        return Files.newBufferedReader(canonicalJPath)
      }
    }
    throw new FileIsNotFound(internalPath)
  }
}

package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import java.io.{Reader, PrintStream}
import java.nio.file.{FileSystems, Files, Paths, Path => JPath}
import scala.collection.JavaConverters._

// SplitterSprite3でのディレクトリ構造は以下の構造とする
//
// game/ (ゲームディレクトリ, 名称自由)
//   ver1.0.0/ (ヴァージョンディレクトリ、verA.B.Cのフォーマット)
//     game.jar (ver1.0.0のゲーム実行ファイル)
//     aaa.xml (設定用XMLファイル)
//     foo/
//       bbb.xml (設定用XMLファイル)
//     bar/
//       ccc.xml (設定用XMLファイル)
//   patch_from_ver1.0.0_to_ver1.0.1/ (パッチディレクトリ,
//                                     patch_from_verA.B.C_to_verX.Y.Zの
//                                     フォーマット)
//     game.jar (ver1.0.1のゲーム実行ファイル)
//     foo/
//       bbb.xml (設定用XMLファイル)
//   patch_from_ver1.0.1_to_ver1.1.0/ (パッチディレクトリ)
//     game.jar (ver1.1.0のゲーム実行ファイル)
//     bar/
//       ccc.xml (設定用XMLファイル)
//   game.bat (ヴァージョンディレクトリもしくはパッチディレクトリから
//             最新のゲーム実行ファイルを探し、実行するバッチファイル)
//
// Pathクラスはヴァージョンディレクトリもしくはパッチディレクトリを起点とした
// 相対パスを扱うものである。この相対パスを内部パス(internal path)と呼ぶ。
// 上記の例ではPath("aaa.xml")はgame/ver1.0.0/aaa.xmlのみを、
// Path("foo/bbb.xml")はgame/ver1.0.0/foo/bbb.xmlと
// game/patch_from_ver1.0.0_to_ver1.0.1/foo/bbb.xmlを、
// Path("bar/ccc.xml")はgame/ver1.0.0/bar/ccc.xmlと
// game/patch_from_ver1.0.1_to_ver1.1.0/bar/ccc.xmlを表す。
//
// SpawnerがXMLを読み込む際には最新のヴァージョンディレクトリもしくは
// パッチディレクトリのXML読み込む。これにより、パッチディレクトリを
// 置くだけでパッチが適用される。
abstract class IOUtils {
  class InvalidVersionName(val name: String)
    extends Exception(s"ヴァージョン名として${name}は不正です。")
  class InvalidVersionDirectory(val path: JPath)
    extends Exception(s"ヴァージョンディレクトリとして${path}は不正です。")
  class InvalidPatchDirectory(val path: JPath)
    extends Exception(s"パッチディレクトリとして${path}は不正です。")
  class InvaldPatchList(val path: JPath)
    extends Exception(s"${path}へのパッチディレクトリの配置が不正です。")
  class FileIsNotFound(val patchablePath: String)
    extends Exception(s"${patchablePath}にファイルが存在しません。")

  // 標準エラー出力ストリーム
  val stderr: PrintStream

  // 実行ファイルのパス
  val gameJarPath: JPath

  def verOrPatchDirPath: JPath = gameJarPath.getParent()
  def gameDirPath: JPath = verOrPatchDirPath.getParent()
  def appliedPatchDirList: List[JPath] =
    appliedPatchDirListTo(verOrPatchDirPath)

  def tailNameOf(jPath: JPath): String =
    jPath.getName(jPath.getNameCount() - 1).toString

  // "verA.B.C"を(A, B, C)の整数３つ組にパース
  def parseVersionName(name: String): (Int, Int, Int) = {
    if (!name.startsWith("ver")) { throw new InvalidVersionName(name) }
    val splitNumberStrs = name.drop("ver".length).split('.')
    if (splitNumberStrs.length != 3) { throw new InvalidVersionName(name) }

    (splitNumberStrs(0).toInt, splitNumberStrs(1).toInt,
    splitNumberStrs(2).toInt)
  }

  // "verA.B.C"を(A, B, C)の整数３つ組にパース
  def parseVersionDirectory(jPath: JPath): (Int, Int, Int) = try {
    parseVersionName(tailNameOf(jPath))
  } catch {
    case e: InvalidVersionName => throw new InvalidVersionDirectory(jPath)
  }

  // "patch_from_verA.B.C_to_verX.Y.Z"を((A, B, C), (X, Y, Z))にパース
  def parsePatchDirectory(jPath: JPath): ((Int, Int, Int), (Int, Int, Int)) =
    try {
      val name = tailNameOf(jPath)
      if (!name.startsWith("patch_from_")) {
        throw new InvalidPatchDirectory(jPath)
      }
      val splitStrs = name.split('_')
      if (splitStrs.length != 5) { throw new InvalidPatchDirectory(jPath) }
      if (splitStrs(3) != "to") { throw new InvalidPatchDirectory(jPath) }

      val fromVersionIndex = 2
      val toVersionIndex = 4

      (parseVersionName(splitStrs(fromVersionIndex)),
      parseVersionName(splitStrs(toVersionIndex)))
    } catch {
      case e: InvalidVersionName => throw new InvalidPatchDirectory(jPath)
    }

  def isVersionDirectory(jPath: JPath): Boolean =
    Files.isDirectory(jPath) && (try {
      parseVersionDirectory(jPath)
      true
    } catch {
      case e: InvalidVersionDirectory => false
    })

  def isPatchDirectory(jPath: JPath): Boolean =
    Files.isDirectory(jPath) && (try {
      parsePatchDirectory(jPath)
      true
    } catch {
      case e: InvalidPatchDirectory => false
    })

  def findPrevVerOrPatchDir(
      version: (Int, Int, Int), dirIter: Iterator[JPath]): Option[JPath] = {
    if (dirIter.hasNext) {
      val dir = dirIter.next
      val versionIsMatched =
        isVersionDirectory(dir) && parseVersionDirectory(dir) == version
      val patchIsMatched =
        isPatchDirectory(dir) && parsePatchDirectory(dir)._2 == version

      if (versionIsMatched || patchIsMatched) {
        Some(dir)
      } else {
        findPrevVerOrPatchDir(version, dirIter)
      }
    } else {
      None
    }
  }

  def appliedPatchDirListTo(jPath: JPath): List[JPath] = {
    if (isVersionDirectory(jPath)) {
      List(jPath)
    } else if (isPatchDirectory(jPath)) {
      val version = parsePatchDirectory(jPath)._1
      val dirIter = Files.newDirectoryStream(jPath).iterator().asScala
      val prevJPathOpt = findPrevVerOrPatchDir(version, dirIter)
      prevJPathOpt match {
        case Some(prevJPath) => jPath :: appliedPatchDirListTo(prevJPath)
        case None => throw new InvaldPatchList(jPath)
      }
    } else {
      throw new InvaldPatchList(jPath)
    }
  }

  def searchPatchedFile(patchablePath: String): JPath = {
    val separator = FileSystems.getDefault().getSeparator()
    val internalJPath = Paths.get(patchablePath.replace("/", separator))
    appliedPatchDirList.map(_.resolve(internalJPath)).find(Files.exists(_))
      .getOrElse { throw new FileIsNotFound(patchablePath) }
  }

  def withPatchedReader[T](patchablePath: String)(op: Reader => T): T = {
    // UTF-8として読み込み
    val reader = Files.newBufferedReader(searchPatchedFile(patchablePath))
    try { op(reader) } finally { reader.close() }
  }
}

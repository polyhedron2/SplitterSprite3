package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import java.io.{Reader, PrintStream}
import java.nio.file.{FileSystems, Files, Paths, Path => JPath}
import scala.collection.JavaConverters._

// SplitterSprite3でのディレクトリ構造は以下の構造とする
//
// game/ (ゲームディレクトリ, 名称自由)
//   ver1.0.0/ (バージョンディレクトリ、verA.B.Cのフォーマット)
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
//   game.bat (バージョンディレクトリもしくはパッチディレクトリから
//             最新のゲーム実行ファイルを探し、実行するバッチファイル)
//
// Pathクラスはバージョンディレクトリもしくはパッチディレクトリを起点とした
// 相対パスを扱うものである。この相対パスを内部パス(internal path)と呼ぶ。
// 上記の例ではPath("aaa.xml")はgame/ver1.0.0/aaa.xmlのみを、
// Path("foo/bbb.xml")はgame/ver1.0.0/foo/bbb.xmlと
// game/patch_from_ver1.0.0_to_ver1.0.1/foo/bbb.xmlを、
// Path("bar/ccc.xml")はgame/ver1.0.0/bar/ccc.xmlと
// game/patch_from_ver1.0.1_to_ver1.1.0/bar/ccc.xmlを表す。
//
// SpawnerがXMLを読み込む際には最新のバージョンディレクトリもしくは
// パッチディレクトリのXML読み込む。これにより、パッチディレクトリを
// 置くだけでパッチが適用される。

object IOUtils {
  class InvalidVersionName(val name: String)
    extends Exception(s"バージョン名として${name}は不正です。")
  class InvalidVersionDirectory(val path: JPath)
    extends Exception(s"バージョンディレクトリとして${path}は不正です。")
  class InvalidPatchDirectory(val path: JPath)
    extends Exception(s"パッチディレクトリとして${path}は不正です。")
  class InvalidPatchList(val path: JPath)
    extends Exception(s"${path}へのパッチディレクトリの配置が不正です。")
  class FileIsNotFound(val patchablePath: String)
    extends Exception(s"${patchablePath}にファイルが存在しません。")
}

abstract class IOUtils {
  // 標準エラー出力ストリーム
  val stderr: PrintStream

  // 実行ファイルのパス
  val gameJarPath: JPath

  lazy val verOrPatchDirPath: JPath = gameJarPath.getParent()
  lazy val gameDirPath: JPath = verOrPatchDirPath.getParent()

  lazy val version: (Int, Int, Int) =
    if (isVersionDirectory(verOrPatchDirPath)) {
      parseVersionDirectory(verOrPatchDirPath)
    } else if (isPatchDirectory(verOrPatchDirPath)) {
      parsePatchDirectory(verOrPatchDirPath)._2
    } else {
      throw new IOUtils.InvalidPatchList(verOrPatchDirPath)
    }

  lazy val versionName = s"ver${version._1}.${version._2}.${version._3}"

  lazy val appliedPatchDirList: List[JPath] =
    appliedPatchDirList(verOrPatchDirPath, version)

  def tailNameOf(jPath: JPath): String =
    jPath.getName(jPath.getNameCount() - 1).toString

  def hasZenkaku(str: String): Boolean = str.length != str.getBytes().length

  def childrenList(jPath: JPath): List[JPath] = {
    if (Files.isDirectory(jPath)) {
      val stream = Files.newDirectoryStream(jPath)
      try { stream.iterator().asScala.toList } finally { stream.close() }
    } else { Nil }
  }

  // "verA.B.C"を(A, B, C)の整数３つ組にパース
  def parseVersionName(name: String): (Int, Int, Int) = {
    if (hasZenkaku(name) || !name.startsWith("ver")) {
      throw new IOUtils.InvalidVersionName(name)
    }
    val splitNumberStrs = name.drop("ver".length).split('.')

    // 数値は３つ
    if (splitNumberStrs.length != 3) {
      throw new IOUtils.InvalidVersionName(name)
    }

    def parseVersionNum(index: Int): Int = {
      val numberStr = splitNumberStrs(index)
      val num = try { numberStr.toInt } catch {
        case e: NumberFormatException =>
          throw new IOUtils.InvalidVersionName(name)
      }
      // 数値は非負の整数であり一意的でない整数表現は受け付けない
      if (num < 0 || num.toString != numberStr) {
        throw new IOUtils.InvalidVersionName(name)
      }
      num
    }

    (parseVersionNum(0), parseVersionNum(1), parseVersionNum(2))
  }

  // "verA.B.C"を(A, B, C)の整数３つ組にパース
  def parseVersionDirectory(jPath: JPath): (Int, Int, Int) = try {
    parseVersionName(tailNameOf(jPath))
  } catch {
    case e: IOUtils.InvalidVersionName =>
      throw new IOUtils.InvalidVersionDirectory(jPath)
  }

  // "patch_from_verA.B.C_to_verX.Y.Z"を((A, B, C), (X, Y, Z))にパース
  def parsePatchDirectory(jPath: JPath): ((Int, Int, Int), (Int, Int, Int)) =
    try {
      val name = tailNameOf(jPath)

      // 全角文字を混入してはいけない
      if (hasZenkaku(name)) { throw new IOUtils.InvalidPatchDirectory(jPath) }

      if (!name.startsWith("patch_from_")) {
        throw new IOUtils.InvalidPatchDirectory(jPath)
      }
      val splitStrs = name.split('_')
      if (splitStrs.length != 5) {
        throw new IOUtils.InvalidPatchDirectory(jPath)
      }
      if (splitStrs(3) != "to") {
        throw new IOUtils.InvalidPatchDirectory(jPath)
      }

      val fromVersionIndex = 2
      val toVersionIndex = 4

      val fromVersion = parseVersionName(splitStrs(fromVersionIndex))
      val toVersion = parseVersionName(splitStrs(toVersionIndex))

      // パッチバージョンは増加させる方向に適用されなければならない
      if (!isIncreasing(fromVersion, toVersion)) {
        throw new IOUtils.InvalidPatchDirectory(jPath)
      }

      (fromVersion, toVersion)
    } catch {
      case e: IOUtils.InvalidVersionName =>
        throw new IOUtils.InvalidPatchDirectory(jPath)
    }

  def isIncreasing(
      fromVersion: (Int, Int, Int), toVersion: (Int, Int, Int)): Boolean =
    fromVersion._1 < toVersion._1 || {
      fromVersion._1 == toVersion._1 && {
        fromVersion._2 < toVersion._2 || {
          fromVersion._2 == toVersion._2 && {
            fromVersion._3 < toVersion._3 } } } }

  def isVersionDirectory(jPath: JPath): Boolean =
    Files.isDirectory(jPath) && (try {
      parseVersionDirectory(jPath)
      true
    } catch {
      case e: IOUtils.InvalidVersionDirectory => false
    })

  def isPatchDirectory(jPath: JPath): Boolean =
    Files.isDirectory(jPath) && (try {
      parsePatchDirectory(jPath)
      true
    } catch {
      case e: IOUtils.InvalidPatchDirectory => false
    })

  // 指定パスが指定バージョンにつながっていればパッチチェーンを返す
  private def appliedPatchDirList(
      jPath: JPath, toVersion: (Int, Int, Int)): List[JPath] =
    if (isVersionDirectory(jPath) &&
        parseVersionDirectory(jPath) == toVersion) {
      List(jPath)
    } else if (isPatchDirectory(jPath) &&
               parsePatchDirectory(jPath)._2 == toVersion) {
      val fromVersion = parsePatchDirectory(jPath)._1

      val dirList = childrenList(gameDirPath)

      // dirList内のディレクトリからパッチチェーンを探す
      def findAppliedPatchDirList(dirList: List[JPath]): List[JPath] =
        dirList match {
          case head :: tail => try {
            appliedPatchDirList(head, fromVersion)
          } catch {
            // 試したディレクトリで失敗したら次のディレクトリに進む
            case e: IOUtils.InvalidPatchList => findAppliedPatchDirList(tail)
          }
          case Nil => throw new IOUtils.InvalidPatchList(jPath)
        }

      jPath :: findAppliedPatchDirList(dirList)
    } else {
      throw new IOUtils.InvalidPatchList(jPath)
    }

  def searchPatchedFile(patchablePath: String): JPath = {
    val separator = FileSystems.getDefault().getSeparator()
    val patchableJPath = Paths.get(patchablePath.replace("/", separator))
    appliedPatchDirList.map(_.resolve(patchableJPath)).find(Files.exists(_))
      .getOrElse { throw new IOUtils.FileIsNotFound(patchablePath) }
  }

  def withPatchedReader[T](patchablePath: String)(op: Reader => T): T = {
    // UTF-8として読み込み
    val reader = Files.newBufferedReader(searchPatchedFile(patchablePath))
    try { op(reader) } finally { reader.close() }
  }
}

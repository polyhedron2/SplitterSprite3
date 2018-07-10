package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import java.io.{PrintStream}
import java.nio.file.{
  Files => NioFiles, Paths => NioPaths, Path => NioPath}
import scala.collection.JavaConverters._

import jp.gr.java_conf.polyhedron.splittersprite3.common

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
// patchablePathはバージョンディレクトリもしくはパッチディレクトリを起点とした
// 相対パスである。 上記の例では"aaa.xml"はgame/ver1.0.0/aaa.xmlのみを、
// "foo/bbb.xml"はgame/ver1.0.0/foo/bbb.xmlと
// game/patch_from_ver1.0.0_to_ver1.0.1/foo/bbb.xmlを、
// "bar/ccc.xml"はgame/ver1.0.0/bar/ccc.xmlと
// game/patch_from_ver1.0.1_to_ver1.1.0/bar/ccc.xmlを表すpatchablePathである。
//
// SpawnerがXMLを読み込む際には最新のバージョンディレクトリもしくは
// パッチディレクトリのXML読み込む。これにより、パッチディレクトリを
// 置くだけでパッチが適用される。

object IOUtils {
  class InvalidVersionName(val name: String)
    extends Exception(s"バージョン名として${name}は不正です。")
  class InvalidVersionDirectory(val path: NioPath)
    extends Exception(s"バージョンディレクトリとして${path}は不正です。")
  class InvalidPatchDirectory(val path: NioPath)
    extends Exception(s"パッチディレクトリとして${path}は不正です。")
  class InvalidPatchList(val path: NioPath)
    extends Exception(s"${path}へのパッチディレクトリの配置が不正です。")
}

abstract class IOUtils {
  // 標準エラー出力ストリーム
  val stderr: PrintStream

  // 実行ファイルのパス
  val gameJarPath: NioPath

  lazy val gameDirPath: NioPath = gameJarPath.getParent().getParent()
  lazy val latestPatch: common.Patch =
    new common.Patch(gameJarPath.getParent().getFileName().toString)

  private var editPatchOpt: Option[common.Patch] = None
  def editPatch: common.Patch = editPatchOpt.getOrElse(latestPatch)
  def editPatch_=(ep: common.Patch) { editPatchOpt = Some(ep) }

  lazy val version: (Int, Int, Int) =
    if (isVersionDirectory(latestPatch.nioPath)) {
      parseVersionDirectory(latestPatch.nioPath)
    } else if (isPatchDirectory(latestPatch.nioPath)) {
      parsePatchDirectory(latestPatch.nioPath)._2
    } else {
      throw new IOUtils.InvalidPatchList(latestPatch.nioPath)
    }

  lazy val versionName = s"ver${version._1}.${version._2}.${version._3}"

  lazy val appliedPatchList: List[common.Patch] =
    appliedPatchList(latestPatch, version)

  def tailNameOf(path: NioPath): String =
    path.getName(path.getNameCount() - 1).toString

  def hasZenkaku(str: String): Boolean = str.length != str.getBytes().length

  def childrenList(path: NioPath): List[NioPath] = {
    if (NioFiles.isDirectory(path)) {
      val stream = NioFiles.newDirectoryStream(path)
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
  def parseVersionDirectory(path: NioPath): (Int, Int, Int) = try {
    parseVersionName(tailNameOf(path))
  } catch {
    case e: IOUtils.InvalidVersionName =>
      throw new IOUtils.InvalidVersionDirectory(path)
  }

  // "patch_from_verA.B.C_to_verX.Y.Z"を((A, B, C), (X, Y, Z))にパース
  def parsePatchDirectory(path: NioPath): ((Int, Int, Int), (Int, Int, Int)) =
    try {
      val name = tailNameOf(path)

      // 全角文字を混入してはいけない
      if (hasZenkaku(name)) { throw new IOUtils.InvalidPatchDirectory(path) }

      if (!name.startsWith("patch_from_")) {
        throw new IOUtils.InvalidPatchDirectory(path)
      }
      val splitStrs = name.split('_')
      if (splitStrs.length != 5) {
        throw new IOUtils.InvalidPatchDirectory(path)
      }
      if (splitStrs(3) != "to") {
        throw new IOUtils.InvalidPatchDirectory(path)
      }

      val fromVersionIndex = 2
      val toVersionIndex = 4

      val fromVersion = parseVersionName(splitStrs(fromVersionIndex))
      val toVersion = parseVersionName(splitStrs(toVersionIndex))

      // パッチバージョンは増加させる方向に適用されなければならない
      if (!isIncreasing(fromVersion, toVersion)) {
        throw new IOUtils.InvalidPatchDirectory(path)
      }

      (fromVersion, toVersion)
    } catch {
      case e: IOUtils.InvalidVersionName =>
        throw new IOUtils.InvalidPatchDirectory(path)
    }

  def isIncreasing(
      fromVersion: (Int, Int, Int), toVersion: (Int, Int, Int)): Boolean =
    fromVersion._1 < toVersion._1 || {
      fromVersion._1 == toVersion._1 && {
        fromVersion._2 < toVersion._2 || {
          fromVersion._2 == toVersion._2 && {
            fromVersion._3 < toVersion._3 } } } }

  def isVersionDirectory(path: NioPath): Boolean = {
    val resolvedPath = gameDirPath.resolve(path)
    NioFiles.isDirectory(resolvedPath) && (try {
      parseVersionDirectory(resolvedPath)
      true
    } catch {
      case e: IOUtils.InvalidVersionDirectory => false
    })
  }

  def isPatchDirectory(path: NioPath): Boolean = {
    val resolvedPath = gameDirPath.resolve(path)
    NioFiles.isDirectory(resolvedPath) && (try {
      parsePatchDirectory(resolvedPath)
      true
    } catch {
      case e: IOUtils.InvalidPatchDirectory => false
    })
  }

  // 指定パスが指定バージョンにつながっていればパッチチェーンを返す
  private def appliedPatchList(
      patch: common.Patch, toVersion: (Int, Int, Int)): List[common.Patch] =
    if (isVersionDirectory(patch.nioPath) &&
        parseVersionDirectory(patch.nioPath) == toVersion) {
      List(patch)
    } else if (isPatchDirectory(patch.nioPath) &&
               parsePatchDirectory(patch.nioPath)._2 == toVersion) {
      val fromVersion = parsePatchDirectory(patch.nioPath)._1

      val dirList = childrenList(gameDirPath)

      // dirList内のディレクトリからパッチチェーンを探す
      def findAppliedPatchList(dirList: List[NioPath]): List[common.Patch] =
        dirList match {
          case head :: tail => try {
            appliedPatchList(
              new common.Patch(head.getFileName().toString), fromVersion)
          } catch {
            // 試したディレクトリで失敗したら次のディレクトリに進む
            case e: IOUtils.InvalidPatchList => findAppliedPatchList(tail)
          }
          case Nil => throw new IOUtils.InvalidPatchList(patch.nioPath)
        }

      patch :: findAppliedPatchList(dirList)
    } else {
      throw new IOUtils.InvalidPatchList(patch.nioPath)
    }
}

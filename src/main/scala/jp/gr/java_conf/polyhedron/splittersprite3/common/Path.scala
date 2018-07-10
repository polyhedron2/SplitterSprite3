package jp.gr.java_conf.polyhedron.splittersprite3.common

import java.nio.file.{
  FileSystems, StandardOpenOption,
  Path => NioPath, Paths => NioPaths, Files => NioFiles}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import java.io.{Reader, Writer}

object Path {
  class InvalidPatchName(patchName: String)
    extends Exception(s"パッチ名${patchName}は不正です。")
  class FileIsNotFound(path: PatchablePath)
    extends Exception(s"${path}にファイルが存在しません。")
}

abstract class Path(pathStr: String) {
  private val separator = FileSystems.getDefault().getSeparator()
  val nioPath = NioPaths.get(pathStr.replace("/", separator)).normalize

  override def toString = nioPath.toString.replace(separator, "/")
  def name = nioPath.getFileName().toString

  override def equals(that: Any): Boolean =
    getClass() == that.getClass() && toString == that.toString
  override def hashCode: Int = toString.hashCode
}

class RelativePath(pathStr: String) extends Path(pathStr)

class PatchablePath(pathStr: String) extends Path(pathStr) {
  def +:(that: RelativePath): PatchablePath = new PatchablePath(
    nioPath.resolve("..").normalize.resolve(that.nioPath).toString)

  def +:(relativePathStr: String): PatchablePath =
    +:(new RelativePath(relativePathStr))

  def -:(that: PatchablePath): RelativePath = new RelativePath(
    nioPath.resolve("..").normalize.relativize(that.nioPath).toString)

  def patched: PatchedPath =
    Atmosphere.ioUtils.appliedPatchList.map(this +: _).find(_.exists)
      .getOrElse { throw new Path.FileIsNotFound(this) }

  def withReader[T](op: Reader => T): T = {
    // UTF-8として存在するうちの最新パッチから読み込み
    val reader = NioFiles.newBufferedReader(patched.nioPath)
    try { op(reader) } finally { reader.close() }
  }

  def inputStream = NioFiles.newInputStream(patched.nioPath)

  def withWriter[T](op: Writer => T): T = {
    val targetPath = this +: Atmosphere.ioUtils.editPatch
    targetPath.create()

    // UTF-8として現在のパッチに書き込み
    val writer =
      NioFiles.newBufferedWriter(targetPath.nioPath, StandardOpenOption.WRITE)
    try { op(writer) } finally { writer.close() }
  }

  def <(that: PatchablePath): Boolean =
    this.nioPath.toString < that.nioPath.toString
}

class Patch(patchName: String) extends Path(
    Atmosphere.ioUtils.gameDirPath.resolve(patchName).toString) {
  // patchNameは区切り文字を含んではならない
  if (name.contains('/')) { throw new Path.InvalidPatchName(patchName) }

  def +:(that: PatchablePath): PatchedPath = new PatchedPath(
    nioPath.resolve(that.nioPath).toString)
}

class PatchedPath(pathStr: String) extends Path(pathStr) {
  def exists: Boolean = NioFiles.exists(nioPath)

  def create() {
    NioFiles.createDirectories(nioPath.getParent())
    if (!exists) { NioFiles.createFile(nioPath) }
  }
}

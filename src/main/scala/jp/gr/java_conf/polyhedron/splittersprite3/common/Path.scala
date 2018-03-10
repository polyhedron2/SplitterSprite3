package jp.gr.java_conf.polyhedron.splittersprite3.common

import java.io.{Reader}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}

// patchablePath: 内部パス、ファイル区切り文字は'/'で統一
class Path(val patchablePath: String) {
  def withReader[T](op: Reader => T): T =
    Atmosphere.ioUtils.withPatchedReader(patchablePath)(op)
}

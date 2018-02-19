package jp.gr.java_conf.polyhedron.splittersprite3.common

import java.io.{Reader}
import java.nio.file.{Files, Paths, FileSystems}

// ゲームのすべての入出力（プレイヤーの操作、ファイル読み書き、乱数、時刻）
// を管理するシングルトン
// 内部パスの実体パスへの変換を行う。
object Channel {
  def reader(internalPath: String): Reader = {
    val separator = FileSystems.getDefault().getSeparator()
    val internalJPath = Paths.get(internalPath.replace("/", separator))
    for (appliedPatchDir <- FileUtils.appliedPatchDirList) {
      val canonicalJPath = appliedPatchDir.resolve(internalJPath)
      if (Files.exists(canonicalJPath)) {
        // UTF-8として読み込み
        return Files.newBufferedReader(canonicalJPath)
      }
    }
    throw new FileIsNotFound(internalPath)
  }
}

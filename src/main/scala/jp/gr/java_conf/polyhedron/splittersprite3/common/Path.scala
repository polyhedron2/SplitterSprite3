package jp.gr.java_conf.polyhedron.splittersprite3.common

import java.io.{Reader}

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
class Path(val internalPath: String) {
  def reader: Reader = throw new UnsupportedOperationException("TODO: 実装")
}

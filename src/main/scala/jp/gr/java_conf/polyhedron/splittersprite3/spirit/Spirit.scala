package jp.gr.java_conf.polyhedron.splittersprite3.spirit

// XMLファイルへの読み書きを定める抽象クラス
abstract class Spirit {
  // ゲームフォルダ内の相対ファイルパス。区切り文字は'/'で統一。
  val internalPath: String

  // XMLファイル名
  def name = internalPath.split('/').last

  // 各リテラル用のアクセサ
  // 例：
  // spirit.stringOf("ほげ")
  // は以下のXMLを定める。「????」は適当な値。
  // <root>
  //   <val field="ほげ">????</val>
  // </root>
  //
  // 値の取得
  // val str = spirit.stringOf("ほげ")
  // 値の取得(値未設定時のデフォルト値指定)
  // val str = spirit.stringOf("ほげ", "ABC")
  // 値の格納
  // spirit.stringOf("ほげ") = "ABC"
  val stringOf: Accessor[String]
  val booleanOf: Accessor[Boolean]
  val intOf: Accessor[Int]
  val doubleOf: Accessor[Double]

  abstract class Accessor[VALUE] {
    def apply(field: String): VALUE
    def apply(field: String, default: VALUE): VALUE
    def update(field: String, value: VALUE): Unit
  }

  // XML内のサブXMLアクセス用Spirit これをInnerSpiritと呼ぶこととする。
  // 例：
  // spirit("ほげ").stringOf("ふが")
  // は以下のXMLを定める。「????」は適当な値。
  // <root>
  //   <inner field="ほげ">
  //     <val field="ふが">????</val>
  //   </inner>
  // </root>
  //
  def apply(field: String): Spirit

  // 値のファイル保存
  def save(): Unit
}

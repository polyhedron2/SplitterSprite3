package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import javafx.scene.image.{Image}
import scala.reflect.{ClassTag}

import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{
  OutermostSpawner
}

// XMLファイルへの読み書きを定める抽象クラス
abstract class Spirit {
  // ゲームフォルダ内の相対ファイルパス。区切り文字は'/'で統一。
  val patchablePath: String

  // XMLファイル名
  def name: String = patchablePath.split('/').last

  // 各リテラル用のアクセサ
  // 例：
  // spirit.string("ほげ")
  // は以下のXMLを定める。「????」は適当な値。
  // <root>
  //   <val field="ほげ">????</val>
  // </root>
  //
  // 値の取得
  // val str = spirit.string("ほげ")
  // 値の取得(値未設定時のデフォルト値指定)
  // val str = spirit.string("ほげ", "ABC")
  // 値の格納
  // spirit.string("ほげ") = "ABC"
  val string: ValueAccessor[String]
  val boolean: ValueAccessor[Boolean]
  val int: ValueAccessor[Int]
  val double: ValueAccessor[Double]

  abstract class ValueAccessor[VALUE] {
    def apply(field: String): VALUE
    def apply(field: String, default: VALUE): VALUE
    def update(field: String, value: VALUE): Unit
  }

  val outermostSpawner: OutermostSpawnerAccessor

  abstract class OutermostSpawnerAccessor {
    def apply[T <: OutermostSpawner[Any]: ClassTag](field: String): T
    def update[T <: OutermostSpawner[Any]: ClassTag](field: String, value: T)
  }

  val image: FileAccessor[Image]

  abstract class FileAccessor[VALUE] {
    def apply(field: String): VALUE
  }

  // XML内のサブXMLアクセス用Spirit これをInnerSpiritと呼ぶこととする。
  // 例：
  // spirit("ほげ").string("ふが")
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

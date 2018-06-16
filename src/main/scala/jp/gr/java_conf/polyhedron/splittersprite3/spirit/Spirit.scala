package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import javafx.scene.image.{Image}
import scala.reflect.{ClassTag}

import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{
  OutermostSpawner, InnerSpawner
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
  val string: LiteralAccessor[String]
  val boolean: LiteralAccessor[Boolean]
  val int: LiteralAccessor[Int]
  val double: LiteralAccessor[Double]

  abstract class LiteralAccessor[LITERAL] {
    def apply(field: String): LITERAL
    def apply(field: String, default: LITERAL): LITERAL
    def update(field: String, value: LITERAL): Unit
  }

  val image: FileAccessor[Image]

  abstract class FileAccessor[FILE_TYPE] {
    def apply(field: String): FILE_TYPE
  }

  val outermostSpawner: OutermostSpawnerAccessor

  abstract class OutermostSpawnerAccessor {
    def apply[T <: OutermostSpawner[Any]: ClassTag](field: String): T
    def update[T <: OutermostSpawner[Any]: ClassTag](field: String, value: T)
  }

  val innerSpawner: InnerSpawnerAccessor

  abstract class InnerSpawnerAccessor {
    def apply[T <: InnerSpawner[Any]: ClassTag](field: String): T
    def update[T <: InnerSpawner[Any]: ClassTag](field: String, value: T)
  }

  def withString: TypeDefiner1[String]
  def withOutermostSpawner: OutermostTypeDefinerCache

  trait OutermostTypeDefinerCache {
    def apply[T1 <: OutermostSpawner[Any]: ClassTag]: TypeDefiner1[T1]
  }

  trait TypeDefiner1[T1] extends TypeDefiner2[String, T1] {
    def andInnerSpawner: InnerTypeDefinerCache

    trait InnerTypeDefinerCache {
      def apply[T2 <: InnerSpawner[Any]: ClassTag]: TypeDefiner2[T1, T2]
    }

    def seq(field: String): Seq[T1] = kvSeq(field).map(_._2)
    def set(field: String): Set[T1] = seq(field).toSet
  }

  trait TypeDefiner2[T1, T2] {
    def kvSeq: KVAccessor[T1, T2]
    def map(field: String): Map[T1, T2] = kvSeq(field).toMap
  }

  trait KVAccessor[T1, T2] {
    def apply(field: String): Seq[(T1, T2)]
    def update(field: String, value: Seq[(T1, T2)]): Unit
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

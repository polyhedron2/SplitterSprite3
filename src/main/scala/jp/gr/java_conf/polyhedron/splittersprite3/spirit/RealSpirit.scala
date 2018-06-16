package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import java.nio.file.{Paths}
import javafx.scene.image.{Image}
import scala.reflect.{ClassTag}
import scala.xml.{Node}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.common
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{
  Spawner, OutermostSpawner, InnerSpawner}

class SpiritValueIsNotFound(patchablePath: String, field: String)
  extends Exception(s"${patchablePath}[${field}]の値が見つかりません。")
class SpiritValueIsInvalid(
  patchablePath: String, field: String, cause: Exception)
  extends Exception(s"${patchablePath}[${field}]の値が不正です。", cause)
class SpawnerProcessingLoopException(patchablePath: String)
  extends Exception(s"${patchablePath}のSpawnerが循環参照しています。")
class SpawnerIsNotDefined(patchablePath: String)
  extends Exception(s"${patchablePath}のSpawnerが未定義です。")
class SpawnerIsInvalid(
    patchablePath: String, spawnerName: String, cause: Exception)
  extends Exception(s"${patchablePath}のSpawner'${spawnerName}'が不正です。")

// XMLファイルに実際に読み書きを実行する抽象クラス
// 拡張子は*.spirit
abstract class RealSpirit extends Spirit {
  // XMLファイルアクセスロック用オブジェクト
  val lock: AnyRef
  def xml: Node

  // XMLファイル上に指定のfieldで文字列があればSomeでそれを返し、なければNone
  private def rawOpt(element: String, field: String) =
    (xml \ element).find(_.\("@field").text == field).map(_.text)

  def fieldSeq(element: String) = (xml \ element).map(_.\("@field").text)

  // spawner取得処理の無限ループ検出用
  private var isProcessingSpawner = false

  // このSpiritからSpawnするSpawner
  def spawner: Spawner[Any] = if (isProcessingSpawner) {
    throw new SpawnerProcessingLoopException(patchablePath)
  } else {
    val clsPath = rawOpt("spawner", "spawner").getOrElse {
      throw new SpawnerIsNotDefined(patchablePath)
    }

    val constructor = try {
      val cls = Class.forName(clsPath)
      cls.getConstructor(classOf[Spirit])
    } catch {
      case e: Exception =>
        throw new SpawnerIsInvalid(patchablePath, clsPath, e)
    }

    try {
      isProcessingSpawner = true
      constructor.newInstance(this).asInstanceOf[Spawner[Any]]
    } finally {
      isProcessingSpawner = false
    }
  }

  val string = new RealLiteralAccessor[String] {
    val element = "string"
    def raw2Literal(raw: String) = raw
    def literal2Raw(value: String) = value
  }

  val boolean = new RealLiteralAccessor[Boolean] {
    val element = "boolean"
    def raw2Literal(raw: String) = raw.toBoolean
    def literal2Raw(value: Boolean) = value.toString
  }

  val int = new RealLiteralAccessor[Int] {
    val element = "int"
    def raw2Literal(raw: String) = raw.toInt
    def literal2Raw(value: Int) = value.toString
  }

  val double = new RealLiteralAccessor[Double] {
    val element = "double"
    def raw2Literal(raw: String) = raw.toDouble
    def literal2Raw(value: Double) = value.toString
  }

  abstract class RealLiteralAccessor[LITERAL]
      extends LiteralAccessor[LITERAL] {
    def element: String

    // XML上の文字列から指定のリテラルに変換
    def raw2Literal(raw: String): LITERAL
    // 指定のリテラルから文字列に変換
    def literal2Raw(value: LITERAL): String

    def apply(field: String): LITERAL = lock.synchronized {
      try {
        rawOpt(element, field).map(raw2Literal)
      } catch {
        // 文字列をリテラルに変換できなかった場合
        case e: Exception =>
          throw new SpiritValueIsInvalid(patchablePath, field, e)
      }
    } getOrElse {
      // 文字列が設定されていなかった場合
      throw new SpiritValueIsNotFound(patchablePath, field)
    }

    def apply(field: String, default: LITERAL): LITERAL = try {
      apply(field)
    } catch {
      // 文字列が設定されていなかった場合
      case e: SpiritValueIsNotFound => default
    }

    def update(field: String, value: LITERAL) {
      throw new UnsupportedOperationException("TODO: 実装")
    }
  }

  def resolveRelativePath(relativePath: String): String =
    Paths.get(patchablePath).resolve("..").resolve(
      relativePath).normalize.toString

  val image = new RealFileAccessor[Image] {
    val element = "image"
    def path2Value(patchablePath: String) =
      new Image(Atmosphere.ioUtils.inputStream(patchablePath))
  }

  abstract class RealFileAccessor[FILE_TYPE] extends FileAccessor[FILE_TYPE] {
    val element: String
    def path2Value(patchablePath: String): FILE_TYPE

    def apply(field: String): FILE_TYPE =
      lock.synchronized {
        try {
          rawOpt(element, field).map(resolveRelativePath).map(path2Value)
        } catch {
          // 文字列をSpawnerに変換できなかった場合
          case e: Exception =>
            throw new SpiritValueIsInvalid(patchablePath, field, e)
        }
      } getOrElse {
        // 文字列が設定されていなかった場合
        throw new SpiritValueIsNotFound(patchablePath, field)
      }
  }

  val outermostSpawner = new OutermostSpawnerAccessor {
    def apply[T <: OutermostSpawner[Any]: ClassTag](field: String): T =
      lock.synchronized {
        try {
          rawOpt("outermost", field).map(resolveRelativePath).map(
            OutermostRealSpirit(_)).map(_.spawner.asInstanceOf[T])
        } catch {
          // 文字列をSpawnerに変換できなかった場合
          case e: Exception =>
            throw new SpiritValueIsInvalid(patchablePath, field, e)
        }
      } getOrElse {
        // 文字列が設定されていなかった場合
        throw new SpiritValueIsNotFound(patchablePath, field)
      }

    def update[T <: OutermostSpawner[Any]: ClassTag](field: String, value: T) {
      throw new UnsupportedOperationException("TODO: 実装")
    }
  }

  val innerSpawner = new InnerSpawnerAccessor {
    def apply[T <: InnerSpawner[Any]: ClassTag](field: String): T =
      lock.synchronized {
        try {
          innerSpiritMap(field).spawner.asInstanceOf[T]
        } catch {
          // InnerRealSpiritをSpawnerに変換できなかった場合
          case e: Exception =>
            throw new SpiritValueIsInvalid(patchablePath, field, e)
        }
      }

    def update[T <: InnerSpawner[Any]: ClassTag](field: String, value: T) {
      throw new UnsupportedOperationException("TODO: 実装")
    }
  }

  def withString: RealTypeDefiner1[String] =
    new RealTypeDefiner1[String](x => x)
  def withOutermostSpawner[
      T1 <: OutermostSpawner[Any]: ClassTag]: RealTypeDefiner1[T1] =
    new RealTypeDefiner1[T1]({ case relativePath =>
      OutermostRealSpirit(
        resolveRelativePath(relativePath)).spawner.asInstanceOf[T1]
    })

  class RealTypeDefiner1[T1](raw2Key: String => T1)
      extends RealTypeDefiner2SimpleValue[String, T1](x => x, raw2Key)
      with TypeDefiner1[T1] {
    def andInnerSpawner[T2 <: InnerSpawner[Any]: ClassTag]:
        RealTypeDefiner2SpiritValue[T1, T2] =
      new RealTypeDefiner2SpiritValue[T1, T2](
        raw2Key, spirit => spirit.spawner.asInstanceOf[T2])
  }

  class RealTypeDefiner2SimpleValue[T1, T2](
        raw2Key: String => T1, raw2Value: String => T2)
      extends TypeDefiner2[T1, T2] {
    def kvSeq: RealKVAccessorSimpleValue[T1, T2] =
      new RealKVAccessorSimpleValue[T1, T2](raw2Key, raw2Value)
  }

  class RealTypeDefiner2SpiritValue[T1, T2](
        raw2Key: String => T1, spirit2Value: RealSpirit => T2)
      extends TypeDefiner2[T1, T2] {
    def kvSeq: RealKVAccessorSpiritValue[T1, T2] =
      new RealKVAccessorSpiritValue[T1, T2](raw2Key, spirit2Value)
  }

  class RealKVAccessorSimpleValue[T1, T2](
        raw2Key: String => T1, raw2Value: String => T2)
      extends KVAccessor[T1, T2] {
    def apply(field: String): Seq[(T1, T2)] = lock.synchronized {
      val kvSpirit = innerSpiritMap(field)
      val entryFieldSeq = kvSpirit.fieldSeq("key-value")
      entryFieldSeq.map { case entryField =>
        val key = raw2Key(entryField)
        val value = {
          rawOpt("key-value", entryField).map(raw2Value)
        } getOrElse {
          // 文字列が設定されていなかった場合
          throw new SpiritValueIsNotFound(patchablePath, field)
        }

        (key, value)
      }
    }

    def update(field: String, value: Seq[(T1, T2)]) {
      throw new UnsupportedOperationException("TODO: 実装")
    }
  }

  class RealKVAccessorSpiritValue[T1, T2](
        raw2Key: String => T1, spirit2Value: RealSpirit => T2)
      extends KVAccessor[T1, T2] {
    def apply(field: String): Seq[(T1, T2)] = lock.synchronized {
      val kvSpirit = innerSpiritMap(field)
      val entryFieldSeq = kvSpirit.fieldSeq("key-value")
      entryFieldSeq.map { case entryField =>
        val key = raw2Key(entryField)
        val value = spirit2Value(kvSpirit(entryField))
        (key, value)
      }
    }

    def update(field: String, value: Seq[(T1, T2)]) {
      throw new UnsupportedOperationException("TODO: 実装")
    }
  }

  // InnerSpirit一覧管理用
  val innerSpiritMap = new common.Cache[String, InnerRealSpirit] {
    def calc(field: String) = new InnerRealSpirit(RealSpirit.this, field)
  }
  def apply(field: String): RealSpirit = innerSpiritMap(field)
}

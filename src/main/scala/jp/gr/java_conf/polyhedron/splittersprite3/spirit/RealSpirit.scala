package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import java.nio.file.{Paths}
import scala.reflect.{ClassTag}
import scala.xml.{Node}

import jp.gr.java_conf.polyhedron.splittersprite3.common
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{
  Spawner, OutermostSpawner}

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
abstract class RealSpirit extends Spirit {
  // XMLファイルアクセスロック用オブジェクト
  val lock: AnyRef
  def xml: Node

  private def rawValueOpt(element: String, field: String) =
    (xml \ element).find(_.\("@field").text == field).map(_.text)

  // XMLファイル上に指定のfieldで文字列があればSomeでそれを返し、なければNone
  def rawValueOpt(field: String): Option[String] = rawValueOpt("value", field)

  // spawner取得処理の無限ループ検出用
  private var isProcessingSpawner = false

  // このSpiritからSpawnするSpawner
  def spawner: Spawner[Any] = if (isProcessingSpawner) {
    throw new SpawnerProcessingLoopException(patchablePath)
  } else {
    val clsPath = rawValueOpt("spawner", "spawner").getOrElse {
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

  val string = new RealValueAccessor[String] {
    def rawValue2Value(rawValue: String) = rawValue
    def value2RawValue(value: String) = value
  }

  val boolean = new RealValueAccessor[Boolean] {
    def rawValue2Value(rawValue: String) = rawValue.toBoolean
    def value2RawValue(value: Boolean) = value.toString
  }

  val int = new RealValueAccessor[Int] {
    def rawValue2Value(rawValue: String) = rawValue.toInt
    def value2RawValue(value: Int) = value.toString
  }

  val double = new RealValueAccessor[Double] {
    def rawValue2Value(rawValue: String) = rawValue.toDouble
    def value2RawValue(value: Double) = value.toString
  }

  abstract class RealValueAccessor[VALUE] extends ValueAccessor[VALUE] {
    // XML上の文字列から指定のリテラルに変換
    def rawValue2Value(rawValue: String): VALUE
    // 指定のリテラルから文字列に変換
    def value2RawValue(value: VALUE): String

    def apply(field: String): VALUE = lock.synchronized {
      try {
        rawValueOpt(field).map(rawValue2Value)
      } catch {
        // 文字列をリテラルに変換できなかった場合
        case e: Exception =>
          throw new SpiritValueIsInvalid(patchablePath, field, e)
      }
    } getOrElse {
      // 文字列が設定されていなかった場合
      throw new SpiritValueIsNotFound(patchablePath, field)
    }

    def apply(field: String, default: VALUE): VALUE = try {
      apply(field)
    } catch {
      // 文字列が設定されていなかった場合
      case e: SpiritValueIsNotFound => default
    }

    def update(field: String, value: VALUE) {
      throw new UnsupportedOperationException("TODO: 実装")
    }
  }

  def resolveRelativePath(relativePath: String): String =
    Paths.get(patchablePath).resolve("..").resolve(
      relativePath).normalize.toString

  val outermostSpawner = new OutermostSpawnerAccessor {
    def apply[T <: OutermostSpawner[Any]: ClassTag](field: String): T =
      lock.synchronized {
        try {
          rawValueOpt(field).map(resolveRelativePath).map(
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

  // InnerSpirit一覧管理用
  val innerSpiritMap = new common.Cache[String, InnerRealSpirit] {
    def calc(field: String) = new InnerRealSpirit(RealSpirit.this, field)
  }
  def apply(field: String): RealSpirit = innerSpiritMap(field)
}

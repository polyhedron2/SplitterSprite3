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

  // 値の継承元となる親スピリット
  def parentOpt: Option[RealSpirit]
  def withoutParent[T](op: => T): T

  // XMLファイル上に指定のfieldで文字列があればSomeでそれを返し、なければNone
  // 親スピリットも確認
  protected def rawOpt(element: String, field: String): Option[String] =
    (xml \ element).find(_.\("@field").text == field).map(_.text).orElse {
      parentOpt.flatMap(_.rawOpt(element, field))
    }

  protected def fieldSet(element: String): Set[String] =
    (xml \ element).map(_.\("@field").text).toSet ++
    parentOpt.map(_.fieldSet(element)).getOrElse(Set())

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
          loadOutermostSpiritOpt(
            "outermost", field).map(_.spawner.asInstanceOf[T])
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

  protected def loadOutermostSpiritOpt(element: String, field: String) = rawOpt(
    element, field).map(resolveRelativePath).map(OutermostRealSpirit(_))

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

  val withString: RealTypeDefiner1[String] =
    new RealTypeDefiner1[String](x => x, "string", (a, b) => a < b)

  val withOutermostSpawner = new OutermostTypeDefinerCache {
    private var cache =
      Map[Class[_ <: OutermostSpawner[Any]], RealTypeDefiner1[_]]()
    def apply[T1 <: OutermostSpawner[Any]: ClassTag] = {
      val spawnerCls = Atmosphere.reflectionUtils.typeOf[T1]
      if (!cache.isDefinedAt(spawnerCls)) {
        cache += spawnerCls -> new RealTypeDefiner1[T1]({ case relativePath =>
          OutermostRealSpirit(
            resolveRelativePath(relativePath)).spawner.asInstanceOf[T1]
        },
        "outermost", (a, b) => a.spirit.patchablePath < b.spirit.patchablePath)
      }
      cache(spawnerCls).asInstanceOf[RealTypeDefiner1[T1]]
    }
  }

  lazy val permutation = new PermutationAccessor()

  class PermutationAccessor() {
    def apply[T](raw2Key: String => T): common.Permutation[T] = {
      // 親のスピリット抜きでのPermutation
      val thisPerm = withoutParent { rawOpt("permutation", "permutation") }.map(
          common.Permutation.fromString(_, raw2Key)).getOrElse {
        // 設定がなければ恒等置換
        new common.Permutation(Map())
      }
      // 親スピリットから祖先分をすべて合成したPermutation
      val parentPerm = parentOpt.map(_.permutation(raw2Key)).getOrElse {
        // 設定がなければ恒等置換
        new common.Permutation(Map())
      }
      // 自分を含め祖先分をすべて合成したPermutation
      thisPerm * parentPerm
    }

    def update[T](key2Raw: T => String) {
      throw new UnsupportedOperationException("TODO: 実装")
    }
  }

  class RealTypeDefiner1[T1](
        raw2Key: String => T1, element: String, ordering: (T1, T1) => Boolean)
      extends RealTypeDefiner2SimpleValue[String, T1](
        x => x, raw2Key, element, (a, b) => a < b)
      with TypeDefiner1[T1] {
    val andInnerSpawner = new InnerTypeDefinerCache {
      private var cache = Map[
        Class[_ <: InnerSpawner[Any]], RealTypeDefiner2SpiritValue[T1, _]]()
      def apply[T2 <: InnerSpawner[Any]: ClassTag] = {
        val spawnerCls = Atmosphere.reflectionUtils.typeOf[T2]
        if (!cache.isDefinedAt(spawnerCls)) {
          cache += spawnerCls -> new RealTypeDefiner2SpiritValue[T1, T2](
            raw2Key, sp => sp.spawner.asInstanceOf[T2], ordering)
        }
        cache(spawnerCls).asInstanceOf[RealTypeDefiner2SpiritValue[T1, T2]]
      }
    }
  }

  class RealTypeDefiner2SimpleValue[T1, T2](
        raw2Key: String => T1, raw2Value: String => T2,
        element: String, ordering: (T1, T1) => Boolean)
      extends TypeDefiner2[T1, T2] {
    val kvSeq: RealKVAccessorSimpleValue[T1, T2] =
      new RealKVAccessorSimpleValue[T1, T2](
        raw2Key, raw2Value, element, ordering)
  }

  class RealTypeDefiner2SpiritValue[T1, T2](
        raw2Key: String => T1, spirit2Value: RealSpirit => T2,
        ordering: (T1, T1) => Boolean)
      extends TypeDefiner2[T1, T2] {
    val kvSeq: RealKVAccessorSpiritValue[T1, T2] =
      new RealKVAccessorSpiritValue[T1, T2](raw2Key, spirit2Value, ordering)
  }

  class RealKVAccessorSimpleValue[T1, T2](
        raw2Key: String => T1, raw2Value: String => T2,
        element: String, ordering: (T1, T1) => Boolean)
      extends KVAccessor[T1, T2] {
    def apply(field: String): Seq[(T1, T2)] = lock.synchronized {
      val kvSpirit = innerSpiritMap(field)
      val entryFieldSet = kvSpirit.fieldSet(element)
      val perm = kvSpirit.permutation(raw2Key)

      entryFieldSet.map { case entryField =>
        val key = raw2Key(entryField)
        val value = {
          kvSpirit.rawOpt(element, entryField).map(raw2Value)
        } getOrElse {
          // 文字列が設定されていなかった場合
          throw new SpiritValueIsNotFound(patchablePath, field)
        }
        (key, value)
      }.toSeq.sortWith((kv1, kv2) => (ordering *: perm)(kv1._1, kv2._1))
    }

    def update(field: String, value: Seq[(T1, T2)]) {
      throw new UnsupportedOperationException("TODO: 実装")
    }
  }

  class RealKVAccessorSpiritValue[T1, T2](
        raw2Key: String => T1, spirit2Value: RealSpirit => T2,
        ordering: (T1, T1) => Boolean)
      extends KVAccessor[T1, T2] {
    def apply(field: String): Seq[(T1, T2)] = lock.synchronized {
      val kvSpirit = innerSpiritMap(field)
      val entryFieldSet = kvSpirit.fieldSet("inner")
      val perm = kvSpirit.permutation(raw2Key)

      entryFieldSet.map { case entryField =>
        val key = raw2Key(entryField)
        val value = spirit2Value(kvSpirit(entryField))
        (key, value)
      }.toSeq.sortWith((kv1, kv2) => (ordering *: perm)(kv1._1, kv2._1))
    }

    def update(field: String, value: Seq[(T1, T2)]) {
      throw new UnsupportedOperationException("TODO: 実装")
    }
  }

  // InnerSpirit一覧管理用
  val innerSpiritMap = new common.Cache[String, InnerRealSpirit] {
    def calc(field: String) = new InnerRealSpirit(RealSpirit.this, field)
  }
  def apply(field: String): InnerRealSpirit = innerSpiritMap(field)
}

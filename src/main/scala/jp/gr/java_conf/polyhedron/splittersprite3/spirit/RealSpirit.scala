package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import java.nio.file.{Paths}
import javafx.scene.image.{Image}
import scala.reflect.{ClassTag}
import scala.xml.{Elem, XML}

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
class SpawnerNotHaveSpiritConstructor(
    patchablePath: String, cls: Class[_ <: Spawner[Any]], cause: Exception)
  extends Exception(
    s"${patchablePath}のSpawner ${cls.getName()}は" +
    "スピリットによるコンストラクタを持っていません。")

// XMLファイルに実際に読み書きを実行する抽象クラス
// 拡張子は*.spirit
abstract class RealSpirit extends Spirit {
  // XMLファイルアクセスロック用オブジェクト
  val lock: AnyRef
  def xml: Elem
  def xml_=(newXML: Elem)

  // 値の継承元となる親スピリット
  def parentOpt: Option[RealSpirit]
  def withoutParent[T](op: => T): T

  protected def updatedXML(
      targetXML: Elem, element: String, field: String,
      partialXMLOpt: Option[Elem]): Elem = {
    val child = targetXML.child.filter(
      // 指定のelement, fieldのXML部分の古いものは削除
      node => node.label != element || (node \ "@field").text != field) ++
      // 新しい要素を追加。Noneであれば何もしない
      partialXMLOpt
    // child以外は変更なし
    new Elem(
      targetXML.prefix, targetXML.label, targetXML.attributes, targetXML.scope,
      targetXML.minimizeEmpty,
      child.sortBy(node => (node \ "@field").text).sortBy(_.label):_*)
  }

  // 親スピリットのXMLと合わせたXML
  def compositeXML: Elem = parentOpt.map { case parent =>
    val elementAndFields = xml.child.map(
      node => (node.label, (node \ "@field").text)).toSet

    val innerChild =
      (fieldSet("inner") ++ parent.fieldSet("inner")).toSeq.map { case field =>
        innerSpiritMap(field).compositeXML
      }

    val nonInnerChild =
      parent.compositeXML.child.filter(node =>
        node.label != "inner" &&
        !elementAndFields((node.label, (node \ "@field").text))) ++
      xml.child.filter(node => node.label != "inner")

    val child = nonInnerChild ++ innerChild

    new Elem(
      xml.prefix, xml.label, xml.attributes, xml.scope, xml.minimizeEmpty,
      child.sortBy(node => (node \ "@field").text).sortBy(_.label):_*)
  }.getOrElse {
    xml
  }

  // XMLファイル上に指定のfieldで文字列があればSomeでそれを返し、なければNone
  // 親スピリットも確認
  protected def rawOpt = new RawOptAccessor()

  class RawOptAccessor() {
    def apply(element: String, field: String): Option[String] =
      lock.synchronized {
        (xml \ element).find(_.\("@field").text == field).map(_.text).orElse {
          parentOpt.flatMap(_.rawOpt(element, field))
        }
      }

    def update(element: String, field: String, newRawOpt: Option[String]) {
      lock.synchronized {
        xml = updatedXML(xml, element, field, newRawOpt.map { case raw =>
          "<" + element + " field=\"" + field + "\">" + raw +
          "</" + element + ">"
        }.map(XML.loadString))
      }
    }
  }

  // 指定elementのfield一覧
  protected def fieldSet(element: String): Set[String] =
    (xml \ element).map(_.\("@field").text).toSet ++
    parentOpt.map(_.fieldSet(element)).getOrElse(Set()) --
    (xml \ element).filter(
      _.\("@deleted").text == "true").map(_.\("@field").text)

  // spawner取得処理の無限ループ検出用
  private var isProcessingSpawner = false

  def spawnerClassOpt: Option[Class[_ <: Spawner[Any]]] =
    rawOpt("spawner", "spawner").map { case raw =>
      try {
        Class.forName(raw).asInstanceOf[Class[Spawner[Any]]]
      } catch {
        case e: Exception =>
          throw new SpawnerIsInvalid(patchablePath, raw, e)
      }
    }

  def spawnerClassOpt_=(newSpawnerClassOpt: Option[Class[_ <: Spawner[Any]]]) {
    rawOpt("spawner", "spawner") = newSpawnerClassOpt.map(_.getName())
  }

  // このSpiritからSpawnするSpawner
  def spawner: Spawner[Any] = if (isProcessingSpawner) {
    throw new SpawnerProcessingLoopException(patchablePath)
  } else {
    val cls = spawnerClassOpt.getOrElse {
      throw new SpawnerIsNotDefined(patchablePath)
    }

    val constructor = try {
      cls.getConstructor(classOf[Spirit])
    } catch {
      case e: Exception =>
        throw new SpawnerNotHaveSpiritConstructor(patchablePath, cls, e)
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

    def apply(field: String): LITERAL = {
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
      rawOpt(element, field) = Some(literal2Raw(value))
    }
  }

  def resolve(relativePath: String): String =
    Paths.get(patchablePath).resolve("..").normalize.resolve(
      relativePath).normalize.toString

  def relativize(otherPatchablePath: String): String =
    Paths.get(patchablePath).resolve("..").normalize.relativize(
      Paths.get(otherPatchablePath)).normalize.toString

  val image = new RealFileAccessor[Image] {
    val element = "image"
    def path2Value(patchablePath: String) =
      new Image(Atmosphere.ioUtils.inputStream(patchablePath))
  }

  abstract class RealFileAccessor[FILE_TYPE] extends FileAccessor[FILE_TYPE] {
    val element: String
    def path2Value(patchablePath: String): FILE_TYPE

    def apply(field: String): FILE_TYPE = {
      try {
        rawOpt(element, field).map(resolve).map(path2Value)
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

  def spawner2RelativePath(spawner: OutermostSpawner[Any]) =
    relativize(spawner.spirit.patchablePath)

  val outermostSpawner = new OutermostSpawnerAccessor {
    def apply[T <: OutermostSpawner[Any]: ClassTag](field: String): T = {
      try {
        rawOpt("outermost", field).map(resolve).map(
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
      rawOpt("outermost", field) = Some(spawner2RelativePath(value))
    }
  }

  val innerSpawner = new InnerSpawnerAccessor {
    def apply[T <: InnerSpawner[Any]: ClassTag](field: String): T =
      innerSpiritMap(field).spawner.asInstanceOf[T]

    def update[T <: InnerSpawner[Any]: ClassTag](field: String, value: T) {
      // compositeXMLは元のvalueのフィールド名を持っているので差し替える
      val newInnerXML = XML.loadString(
        "<inner field=\"" + field + "\">" +
        value.spirit.compositeXML.child.mkString +
        "</inner>")
      xml = updatedXML(xml, "inner", field, Some(newInnerXML))
    }
  }

  val withString: RealTypeDefiner1[String] =
    new RealTypeDefiner1[String](x => x, x => x, "string", (a, b) => a < b)

  val withOutermostSpawner = new OutermostTypeDefinerCache {
    private var cache =
      Map[Class[_ <: OutermostSpawner[Any]], RealTypeDefiner1[_]]()
    def apply[T1 <: OutermostSpawner[Any]: ClassTag] = {
      val spawnerCls = Atmosphere.reflectionUtils.typeOf[T1]
      if (!cache.isDefinedAt(spawnerCls)) {
        cache += spawnerCls -> new RealTypeDefiner1[T1](
        relativePath => OutermostRealSpirit(
          resolve(relativePath)).spawner.asInstanceOf[T1],
        spawner2RelativePath,
        "outermost", (a, b) => a.spirit.patchablePath < b.spirit.patchablePath)
      }
      cache(spawnerCls).asInstanceOf[RealTypeDefiner1[T1]]
    }
  }

  def permutation[T](raw2Key: String => T): common.Permutation[T] =
    rawOpt("permutation", "permutation").map(
        common.Permutation.fromString(_, raw2Key)).getOrElse {
      // 設定がなければ恒等置換
      new common.Permutation(Map())
    }

  class RealTypeDefiner1[T1](
        raw2Key: String => T1, key2Raw: T1 => String,
        element: String, ordering: (T1, T1) => Boolean)
      extends RealTypeDefiner2SimpleValue[String, T1](
        x => x, raw2Key, x => x, key2Raw, element, (a, b) => a < b)
      with TypeDefiner1[T1] {
    val andInnerSpawner = new InnerTypeDefinerCache {
      private var cache = Map[
        Class[_ <: InnerSpawner[Any]], RealTypeDefiner2SpiritValue[T1, _]]()
      def apply[T2 <: InnerSpawner[Any]: ClassTag] = {
        val spawnerCls = Atmosphere.reflectionUtils.typeOf[T2]
        if (!cache.isDefinedAt(spawnerCls)) {
          cache += spawnerCls -> new RealTypeDefiner2SpiritValue[T1, T2](
            raw2Key, spirit => spirit.spawner.asInstanceOf[T2],
            key2Raw, spawner => spawner.spirit,
            ordering)
        }
        cache(spawnerCls).asInstanceOf[RealTypeDefiner2SpiritValue[T1, T2]]
      }
    }
  }

  class RealTypeDefiner2SimpleValue[T1, T2](
        raw2Key: String => T1, raw2Value: String => T2,
        key2Raw: T1 => String, value2Raw: T2 => String,
        element: String, ordering: (T1, T1) => Boolean)
      extends TypeDefiner2[T1, T2] {
    val kvSeq: RealKVAccessorSimpleValue[T1, T2] =
      new RealKVAccessorSimpleValue[T1, T2](
        raw2Key, raw2Value, key2Raw, value2Raw, element, ordering)
  }

  class RealTypeDefiner2SpiritValue[T1, T2](
        raw2Key: String => T1, spirit2Value: Spirit => T2,
        key2Raw: T1 => String, value2Spirit: T2 => Spirit,
        ordering: (T1, T1) => Boolean)
      extends TypeDefiner2[T1, T2] {
    val kvSeq: RealKVAccessorSpiritValue[T1, T2] =
      new RealKVAccessorSpiritValue[T1, T2](
        raw2Key, spirit2Value, key2Raw, value2Spirit, ordering)
  }

  class RealKVAccessorSimpleValue[T1, T2](
        val raw2Key: String => T1, raw2Value: String => T2,
        val key2Raw: T1 => String, val value2Raw: T2 => String,
        val element: String, val ordering: (T1, T1) => Boolean)
      extends RealKVAccessor[T1, T2] {
    def valueOpt(kvSpirit: RealSpirit, entryField: String) =
      kvSpirit.rawOpt(element, entryField).map(raw2Value)
  }

  class RealKVAccessorSpiritValue[T1, T2](
        val raw2Key: String => T1, spirit2Value: Spirit => T2,
        val key2Raw: T1 => String, value2Spirit: T2 => Spirit,
        val ordering: (T1, T1) => Boolean)
      extends RealKVAccessor[T1, T2] {
    val element = "inner"
    val value2Raw =
      (value: T2) => value2Spirit(value).compositeXML.child.mkString

    def valueOpt(kvSpirit: RealSpirit, entryField: String) =
      Some(spirit2Value(kvSpirit(entryField)))
  }

  trait RealKVAccessor[T1, T2] extends KVAccessor[T1, T2] {
    val element: String
    val raw2Key: String => T1
    val key2Raw: T1 => String
    val value2Raw: T2 => String
    val ordering: (T1, T1) => Boolean

    def valueOpt(kvSpirit: RealSpirit, entryField: String): Option[T2]

    def apply(field: String): Seq[(T1, T2)] = lock.synchronized {
      val kvSpirit = innerSpiritMap(field)
      val entryFieldSet = kvSpirit.fieldSet(element)
      val perm = kvSpirit.permutation(raw2Key)

      entryFieldSet.flatMap { case entryField =>
        val key = raw2Key(entryField)
        valueOpt(kvSpirit, entryField).map(value => (key, value))
      }.toSeq.sortWith((kv1, kv2) => (ordering *: perm)(kv1._1, kv2._1))
    }

    def update(field: String, value: Seq[(T1, T2)]) {
      lock.synchronized {
        val kvSpirit = innerSpiritMap(field)

        val deletedFields =
          kvSpirit.parentOpt.map(_.fieldSet(element)).getOrElse(Set()) --
          value.map(_._1).map(key2Raw)

        val entries =
          // 親スピリットに比べ、追加・更新された値
          value.map(kv => (key2Raw(kv._1), Some(value2Raw(kv._2)))) ++
          // 親スピリットに比べ、削除された値
          deletedFields.map((_, None))

        val permText = common.Permutation.toString(
          common.Permutation.extract(value.map(_._1), ordering), key2Raw)

        xml = updatedXML(xml, "inner", field, Some(XML.loadString(
          "<inner field=\"" + field + "\">" +
          "<permutation field=\"permutation\">" + permText + "</permutation>" +
          entries.sortBy(_._1).map {
            case (entryKey, Some(entryValue)) =>
              "<" + element + " field=\"" + entryKey + "\">" + entryValue +
              "</" + element + ">"
            case (entryKey, None) =>
              "<" + element + " field=\"" + entryKey + "\" deleted=\"true\"/>"
          }.mkString +
          "</inner>")))
      }
    }
  }

  // InnerSpirit一覧管理用
  protected val innerSpiritMap = new common.Cache[String, InnerRealSpirit] {
    def calc(field: String) = new InnerRealSpirit(RealSpirit.this, field)
  }
  def apply(field: String): InnerRealSpirit = innerSpiritMap(field)
}

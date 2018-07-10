package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import javafx.scene.image.{Image}
import scala.reflect.{ClassTag}
import scala.xml.{Elem, XML, Node}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.common
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{
  Spawner, OutermostSpawner, InnerSpawner}

class SpiritValueIsNotFound(spirit: RealSpirit, field: String)
  extends Exception(s"${spirit}[${field}]の値が見つかりません。")
class SpiritValueIsInvalid(spirit: RealSpirit, field: String, cause: Exception)
  extends Exception(s"${spirit}[${field}]の値が不正です。", cause)
class SpawnerProcessingLoopException(spirit: RealSpirit)
  extends Exception(s"${spirit}のSpawnerが循環参照しています。")
class SpawnerIsNotDefined(spirit: RealSpirit)
  extends Exception(s"${spirit}のSpawnerが未定義です。")
class SpawnerIsInvalid(
    spirit: RealSpirit, spawnerName: String, cause: Exception)
  extends Exception(s"${spirit}のSpawner'${spawnerName}'が不正です。")
class SpawnerNotHaveSpiritConstructor(
    spirit: RealSpirit, cls: Class[_ <: Spawner[Any]], cause: Exception)
  extends Exception(
    s"${spirit}のSpawner ${cls.getName()}は" +
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

  private def sortXMLNodes(nodes: Seq[Node]) =
    nodes.sortBy(node => (node \ "@field").text)
         .sortBy(node => (node \ "@typing").text).sortBy(_.label)

  protected def updatedXML(
      targetXML: Elem, element: String, typing: String, field: String,
      partialXMLOpt: Option[Elem]): Elem = {
    val child = targetXML.child.filter(
      // 指定のelement, typing, fieldのXML部分の古いものは削除
      node => node.label != element ||
      (node \ "@typing").text != typing ||
      (node \ "@field").text != field) ++
      // 新しい要素を追加。Noneであれば何もしない
      partialXMLOpt
    // child以外は変更なし
    new Elem(
      targetXML.prefix, targetXML.label, targetXML.attributes, targetXML.scope,
      targetXML.minimizeEmpty,
      sortXMLNodes(child):_*)
  }

  // scalaのxmlパーザはロード時にattributeを逆転させてしまうので
  // 再ロードして元に戻す
  // 参考：<https://github.com/scala/scala-xml/issues/65>
  protected def loadXMLFromString(xmlText: String): Elem = {
    val middleXML = XML.loadString(xmlText)
    XML.loadString(middleXML.toString)
  }

  // 親スピリットのXMLと合わせたXML
  def compositeXML: Elem = parentOpt.map { case parent =>
    val accessInfos = xml.child.map { case node =>
      (node.label, (node \ "@typing").text, (node \ "@field").text)
    }.toSet

    val innerChild =
      (typingFieldSet("inner") ++ parent.typingFieldSet("inner")).toSeq.map {
        case (typing, field) => innerSpiritMap(typing, field).compositeXML
      }

    val nonInnerChild =
      parent.compositeXML.child.filter(node =>
        node.label != "inner" &&
        !accessInfos(
          (node.label, (node \ "@typing").text, (node \ "@field").text))) ++
      xml.child.filter(node => node.label != "inner")

    val child = nonInnerChild ++ innerChild

    new Elem(
      xml.prefix, xml.label, xml.attributes, xml.scope, xml.minimizeEmpty,
      sortXMLNodes(child):_*)
  }.getOrElse {
    xml
  }

  // XMLファイル上に指定のfieldで文字列があればSomeでそれを返し、なければNone
  // 親スピリットも確認
  protected def rawOpt = new RawOptAccessor()

  class RawOptAccessor() {
    def apply(element: String, typing: String, field: String): Option[String] =
      lock.synchronized {
        (xml \ element)
          .filter(_.\("@typing").text == typing)
          .find(_.\("@field").text == field)
          .map(_.text)
          .orElse(parentOpt.flatMap(_.rawOpt(element, typing, field)))
      }

    def update(element: String, typing: String, field: String,
               newRawOpt: Option[String]) {
      lock.synchronized {
        xml = updatedXML(
          xml, element, typing, field,
          newRawOpt.map { case raw =>
            "<" + element +
            " typing=\"" + typing + "\"" +
            " field=\"" + field + "\"" +
            ">" + raw + "</" + element + ">"
          }.map(loadXMLFromString))
      }
    }
  }

  protected def pathOpt = new PathOptAccessor()

  class PathOptAccessor() {
    def apply(typing: String, field: String): Option[common.PatchablePath] =
      rawOpt("path", typing, field)
        .map(new common.RelativePath(_)).map(_ +: patchablePath)

    def update(typing: String, field: String,
               newPathOpt: Option[common.PatchablePath]) {
      rawOpt("path", typing, field) =
        newPathOpt.map(_ -: patchablePath).map(_.toString)
    }
  }

  // 指定elementとtypingのfield一覧
  protected def fieldSet(element: String, typing: String): Set[String] =
    typingFieldSet(element).filter(_._1 == typing).map(_._2)

  // 指定elementのtypingとfield一覧
  protected def typingFieldSet(element: String): Set[(String, String)] =
    (xml \ element)
      .map(node => ((node \ "@typing").text, (node \ "@field").text)).toSet ++
    parentOpt.map(_.typingFieldSet(element)).getOrElse(Set()) --
    (xml \ element)
      .filter(_.\("@deleted").text == "true")
      .map(node => ((node \ "@typing").text, (node \ "@field").text)).toSet

  // spawner取得処理の無限ループ検出用
  private var isProcessingSpawner = false

  def spawnerClassOpt: Option[Class[_ <: Spawner[Any]]] =
    rawOpt("literal", "special", "spawner").map { case raw =>
      try {
        Class.forName(raw).asInstanceOf[Class[Spawner[Any]]]
      } catch {
        case e: Exception =>
          throw new SpawnerIsInvalid(this, raw, e)
      }
    }

  def spawnerClassOpt_=(newSpawnerClassOpt: Option[Class[_ <: Spawner[Any]]]) {
    rawOpt("literal", "special", "spawner") =
      newSpawnerClassOpt.map(_.getName())
  }

  // このSpiritからSpawnするSpawner
  def spawner: Spawner[Any] = if (isProcessingSpawner) {
    throw new SpawnerProcessingLoopException(this)
  } else {
    val cls = spawnerClassOpt.getOrElse {
      throw new SpawnerIsNotDefined(this)
    }

    val constructor = try {
      cls.getConstructor(classOf[Spirit])
    } catch {
      case e: Exception =>
        throw new SpawnerNotHaveSpiritConstructor(this, cls, e)
    }

    try {
      isProcessingSpawner = true
      constructor.newInstance(this).asInstanceOf[Spawner[Any]]
    } finally {
      isProcessingSpawner = false
    }
  }

  val string = new RealLiteralAccessor[String] {
    val typing = "string"
    def raw2Literal(raw: String) = raw
    def literal2Raw(value: String) = value
  }

  val boolean = new RealLiteralAccessor[Boolean] {
    val typing = "boolean"
    def raw2Literal(raw: String) = raw.toBoolean
    def literal2Raw(value: Boolean) = value.toString
  }

  val int = new RealLiteralAccessor[Int] {
    val typing = "int"
    def raw2Literal(raw: String) = raw.toInt
    def literal2Raw(value: Int) = value.toString
  }

  val double = new RealLiteralAccessor[Double] {
    val typing = "double"
    def raw2Literal(raw: String) = raw.toDouble
    def literal2Raw(value: Double) = value.toString
  }

  abstract class RealLiteralAccessor[LITERAL]
      extends LiteralAccessor[LITERAL] {
    def typing: String

    // XML上の文字列から指定のリテラルに変換
    def raw2Literal(raw: String): LITERAL
    // 指定のリテラルから文字列に変換
    def literal2Raw(value: LITERAL): String

    def apply(field: String): LITERAL = {
      try {
        rawOpt("literal", typing, field).map(raw2Literal)
      } catch {
        // 文字列をリテラルに変換できなかった場合
        case e: Exception =>
          throw new SpiritValueIsInvalid(RealSpirit.this, field, e)
      }
    } getOrElse {
      // 文字列が設定されていなかった場合
      throw new SpiritValueIsNotFound(RealSpirit.this, field)
    }

    def apply(field: String, default: LITERAL): LITERAL = try {
      apply(field)
    } catch {
      // 文字列が設定されていなかった場合
      case e: SpiritValueIsNotFound => default
    }

    def update(field: String, value: LITERAL) {
      rawOpt("literal", typing, field) = Some(literal2Raw(value))
    }
  }

  val image = new RealFileAccessor[Image] {
    val typing = "image"
    def path2Value(path: common.PatchablePath) = new Image(path.inputStream)
  }

  abstract class RealFileAccessor[FILE_TYPE] extends FileAccessor[FILE_TYPE] {
    val typing: String
    def path2Value(path: common.PatchablePath): FILE_TYPE

    def apply(field: String): FILE_TYPE = {
      try {
        pathOpt(typing, field).map(path2Value)
      } catch {
        // 文字列をSpawnerに変換できなかった場合
        case e: Exception =>
          throw new SpiritValueIsInvalid(RealSpirit.this, field, e)
      }
    } getOrElse {
      // 文字列が設定されていなかった場合
      throw new SpiritValueIsNotFound(RealSpirit.this, field)
    }
  }

  val outermostSpawner = new OutermostSpawnerAccessor {
    def apply[T <: OutermostSpawner[Any]: ClassTag](field: String): T = {
      try {
        pathOpt("spirit", field).map(
          OutermostRealSpirit(_)).map(_.spawner.asInstanceOf[T])
      } catch {
        // 文字列をSpawnerに変換できなかった場合
        case e: Exception =>
          throw new SpiritValueIsInvalid(RealSpirit.this, field, e)
      }
    } getOrElse {
      // 文字列が設定されていなかった場合
      throw new SpiritValueIsNotFound(RealSpirit.this, field)
    }

    def update[T <: OutermostSpawner[Any]: ClassTag](field: String, value: T) {
      pathOpt("spirit", field) = Some(value.spirit.patchablePath)
    }
  }

  val innerSpawner = new InnerSpawnerAccessor {
    def apply[T <: InnerSpawner[Any]: ClassTag](field: String): T =
      innerSpiritMap("spirit", field).spawner.asInstanceOf[T]

    def update[T <: InnerSpawner[Any]: ClassTag](field: String, value: T) {
      // compositeXMLは元のvalueのフィールド名を持っているので差し替える
      val newInnerXML = loadXMLFromString(
        "<inner" +
        " typing=\"spirit\"" +
        " field=\"" + field + "\"" +
        ">" + value.spirit.compositeXML.child.mkString + "</inner>")
      xml = updatedXML(xml, "inner", "spirit", field, Some(newInnerXML))
    }
  }

  val withString: RealTypeDefiner1[String] = new RealTypeDefiner1[String](
    x => x, x => x, "literal", "string", (a, b) => a < b)

  val withOutermostSpawner = new OutermostTypeDefinerCache {
    private var cache =
      Map[Class[_ <: OutermostSpawner[Any]], RealTypeDefiner1[_]]()
    def apply[T1 <: OutermostSpawner[Any]: ClassTag] = {
      val spawnerCls = Atmosphere.reflectionUtils.typeOf[T1]
      if (!cache.isDefinedAt(spawnerCls)) {
        cache += (spawnerCls -> new RealTypeDefiner1[T1](
          relativePathStr => OutermostRealSpirit(
            relativePathStr +: patchablePath).spawner.asInstanceOf[T1],
          spawner => (spawner.spirit.patchablePath -: patchablePath).toString,
          "path", "spirit",
          (a, b) => a.spirit.patchablePath < b.spirit.patchablePath))
      }
      cache(spawnerCls).asInstanceOf[RealTypeDefiner1[T1]]
    }
  }

  def permutation[T](raw2Key: String => T): common.Permutation[T] =
    rawOpt("literal", "special", "permutation").map(
        common.Permutation.fromString(_, raw2Key)).getOrElse {
      // 設定がなければ恒等置換
      new common.Permutation(Map())
    }

  class RealTypeDefiner1[T1](
        raw2Key: String => T1, key2Raw: T1 => String,
        element: String, typing: String, ordering: (T1, T1) => Boolean)
      extends RealTypeDefiner2SimpleValue[String, T1](
        x => x, raw2Key, x => x, key2Raw, element, typing, (a, b) => a < b)
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
        element: String, typing: String, ordering: (T1, T1) => Boolean)
      extends TypeDefiner2[T1, T2] {
    val kvSeq: RealKVAccessorSimpleValue[T1, T2] =
      new RealKVAccessorSimpleValue[T1, T2](
        raw2Key, raw2Value, key2Raw, value2Raw, element, typing, ordering)
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
        val element: String, val typing: String,
        val ordering: (T1, T1) => Boolean)
      extends RealKVAccessor[T1, T2] {
    def valueOpt(kvSpirit: RealSpirit, entryField: String) =
      kvSpirit.rawOpt(element, typing, entryField).map(raw2Value)
  }

  class RealKVAccessorSpiritValue[T1, T2](
        val raw2Key: String => T1, spirit2Value: Spirit => T2,
        val key2Raw: T1 => String, value2Spirit: T2 => Spirit,
        val ordering: (T1, T1) => Boolean)
      extends RealKVAccessor[T1, T2] {
    val element = "inner"
    val typing = "spirit"
    val value2Raw =
      (value: T2) => value2Spirit(value).compositeXML.child.mkString

    def valueOpt(kvSpirit: RealSpirit, entryField: String) =
      Some(spirit2Value(kvSpirit(entryField)))
  }

  trait RealKVAccessor[T1, T2] extends KVAccessor[T1, T2] {
    val element: String
    val typing: String
    val raw2Key: String => T1
    val key2Raw: T1 => String
    val value2Raw: T2 => String
    val ordering: (T1, T1) => Boolean

    def valueOpt(kvSpirit: RealSpirit, entryField: String): Option[T2]

    def apply(field: String): Seq[(T1, T2)] = lock.synchronized {
      val kvSpirit = innerSpiritMap("key-value", field)
      val entryFieldSet = kvSpirit.fieldSet(element, typing)
      val perm = kvSpirit.permutation(raw2Key)

      entryFieldSet.flatMap { case entryField =>
        val key = raw2Key(entryField)
        valueOpt(kvSpirit, entryField).map(value => (key, value))
      }.toSeq.sortWith((kv1, kv2) => (ordering *: perm)(kv1._1, kv2._1))
    }

    def update(field: String, value: Seq[(T1, T2)]) {
      lock.synchronized {
        val kvSpirit = innerSpiritMap("key-value", field)

        val deletedFields =
          kvSpirit.parentOpt.map(
            _.fieldSet(element, typing)).getOrElse(Set()) --
          value.map(_._1).map(key2Raw)

        val entries =
          // 親スピリットに比べ、追加・更新された値
          value.map(kv => (key2Raw(kv._1), Some(value2Raw(kv._2)))) ++
          // 親スピリットに比べ、削除された値
          deletedFields.map((_, None))

        val permText = common.Permutation.toString(
          common.Permutation.extract(value.map(_._1), ordering), key2Raw)

        xml = updatedXML(
          xml, "inner", "key-value", field, Some(loadXMLFromString(
            "<inner" +
            " typing=\"key-value\"" +
            " field=\"" + field + "\"" +
            ">" +
            "<literal" +
            " typing=\"special\"" +
            " field=\"permutation\"" +
            ">" + permText +
            "</literal>" +
            entries.sortBy(_._1).map {
              case (entryKey, Some(entryValue)) =>
                "<" + element +
                " typing=\"" + typing + "\"" +
                " field=\"" + entryKey + "\"" +
                ">" +
                entryValue +
                "</" + element + ">"
              case (entryKey, None) =>
                "<" + element +
                " typing=\"" + typing + "\"" +
                " field=\"" + entryKey + "\"" +
                " deleted=\"true\"" +
                "/>"
            }.mkString +
            "</inner>")))
      }
    }
  }

  // InnerSpirit一覧管理用
  val innerSpiritMap =
      new common.Cache[(String, String), InnerRealSpirit] {
    def calc(typingField: (String, String)) =
      new InnerRealSpirit(RealSpirit.this, typingField._1, typingField._2)
  }

  def apply(field: String): InnerRealSpirit = innerSpiritMap("spirit", field)
}

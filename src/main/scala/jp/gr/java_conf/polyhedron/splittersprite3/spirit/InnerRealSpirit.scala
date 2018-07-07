package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import scala.xml.{Elem, XML}

// 他XMLの内部XMLを読み書きするRealSpirit
// outer: １つ外側のRealSpirit
// fieldToThis: outerから見たこのInnerRealSpiritの属するfield
class InnerRealSpirit(outer: RealSpirit, typing: String, fieldToThis: String)
    extends RealSpirit {
  // ロックオブジェクトはOutermostRealSpiritとすることで同一XMLへのアクセスを
  // 管理
  val lock = outer.lock
  val patchablePath = outer.patchablePath

  def xml: Elem = lock.synchronized {
    (outer.xml \ "inner")
          .filter(_.\("@typing").text == typing)
          .find(_.\("@field").text == fieldToThis)
          .map(_.asInstanceOf[Elem])
          .getOrElse {
      // 親がこのInnerRealSpirit用のXMLを持たない場合は空のXMLを読む
      XML.loadString(
        "<inner" +
        " field=\"" + fieldToThis + "\"" +
        " typing=\"" + typing + "\"" +
        "/>")
    }
  }

  def xml_=(newXML: Elem) {
    lock.synchronized {
      outer.xml =
        updatedXML(outer.xml, "inner", typing, fieldToThis, Some(newXML))
    }
  }

  def parentOpt: Option[InnerRealSpirit] =
    outer.parentOpt.map(_.innerSpiritMap(typing, fieldToThis))

  def withoutParent[T](op: => T) = outer.withoutParent(op)

  def save(): Unit = outer.save()

  override def toString = s"${patchablePath}[${fieldToThis}]"
}

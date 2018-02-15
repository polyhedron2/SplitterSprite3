package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import scala.xml.{XML}

// 他XMLの内部XMLを読み書きするRealSpirit
// outer: １つ外側のRealSpirit
// fieldToThis: outerから見たこのInnerRealSpiritの属するfield
class InnerRealSpirit(outer: RealSpirit, fieldToThis: String)
    extends RealSpirit {
  // ロックオブジェクトはOutermostRealSpiritとすることで同一XMLへのアクセスを
  // 管理
  val lock = outer.lock
  val internalPath = s"${outer.internalPath}[${fieldToThis}]"

  def xml =
    (outer.xml \ "inner").find(_.\("@field").text == fieldToThis).getOrElse {
      // 親がこのInnerRealSpirit用のXMLを持たない場合は空のXMLを読む
      XML.loadString("<root/>")
    }

  def save() = outer.save()
}

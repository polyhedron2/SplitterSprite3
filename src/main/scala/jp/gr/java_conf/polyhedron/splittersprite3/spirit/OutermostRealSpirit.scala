package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import scala.xml.{Elem, XML}

import jp.gr.java_conf.polyhedron.splittersprite3.common

// 最も外側のXMLを読み書きするRealSpirit
// １XMLに対してOutermostRealSpiritは１インスタンス
class OutermostRealSpirit(path: common.Path) extends RealSpirit {
  // ロックオブジェクトはOutermostRealSpiritとすることで同一XMLへのアクセスを
  // 管理
  val lock = this
  val internalPath = path.internalPath

  var historyIndex = 0
  var xmlHistory = Map(0 -> XML.load(path.reader))

  def xml = xmlHistory(historyIndex)

  def rawValueOpt(field: String) =
    throw new UnsupportedOperationException("TODO: 実装")

  def save() = throw new UnsupportedOperationException("TODO: 実装")
}

package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import scala.xml.{Elem, XML}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.common

object OutermostRealSpirit {
  private val body = new common.Cache[String, OutermostRealSpirit] {
    def calc(patchablePath: String) = new OutermostRealSpirit(patchablePath)
  }

  def apply(patchablePath: String): OutermostRealSpirit = body(patchablePath)
  def clear() { body.clear() }
}

// 最も外側のXMLを読み書きするRealSpirit
// １XMLに対してOutermostRealSpiritは１インスタンス
// patchablePath: 内部パス、ファイル区切り文字は'/'で統一
class OutermostRealSpirit private (
    val patchablePath: String) extends RealSpirit {
  // ロックオブジェクトはOutermostRealSpiritとすることで同一XMLへのアクセスを
  // 管理
  val lock = this

  // XMLの編集履歴
  var historyIndex = 0
  var xmlHistory = Map(
    0 -> Atmosphere.ioUtils.withPatchedReader(patchablePath)(XML.load))

  def xml: Elem = xmlHistory(historyIndex)

  private var loadParent = true

  lazy val parentOpt: Option[OutermostRealSpirit] = lock.synchronized {
    if (loadParent) {
      withoutParent { loadOutermostSpiritOpt("parent", "parent") }
    } else {
      None
    }
  }

  def withoutParent[T](op: => T): T = {
    val prevFlag = loadParent
    try {
      loadParent = false
      op
    } finally {
      loadParent = prevFlag
    }
  }

  def save() { throw new UnsupportedOperationException("TODO: 実装") }
}

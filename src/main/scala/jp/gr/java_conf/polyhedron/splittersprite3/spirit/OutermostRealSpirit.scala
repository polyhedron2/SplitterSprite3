package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import scala.xml.{Elem, XML, PrettyPrinter}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.common

class ChickenOrEggException(spirit: OutermostRealSpirit) extends Exception({
  val loopInfo = spirit.ancestors.map(_.patchablePath).mkString(" -> ")
  s"スピリットの親子関係がループを起こしています。(${loopInfo})"
})

object OutermostRealSpirit {
  private val body = new common.Cache[String, OutermostRealSpirit] {
    def calc(patchablePath: String) = new OutermostRealSpirit(patchablePath)
  }

  def apply(patchablePath: String,
            requireFile: Boolean = true): OutermostRealSpirit = {
    val ret = body(patchablePath)
    if (requireFile) { ret.load() }
    ret
  }

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
  var xmlHistory = Map[Int, Elem](0 -> <root/>)

  def xml: Elem = lock.synchronized { xmlHistory(historyIndex) }

  def xml_=(newXML: Elem) {
    lock.synchronized {
      xmlHistory += (historyIndex -> newXML)
    }
  }

  private var loadParent = true

  def parentOpt: Option[OutermostRealSpirit] = {
    if (loadParent) {
      withoutParent {
        rawOpt("parent", "parent").map(resolve).map(OutermostRealSpirit(_))
      }
    } else {
      None
    }
  }

  def parentOpt_=(newParentOpt: Option[OutermostRealSpirit]) {
    newParentOpt.foreach { case newParent =>
      val ancestorPaths = newParent.ancestors.map(_.patchablePath).toSet
      if (ancestorPaths(patchablePath)) {
        throw new ChickenOrEggException(this)
      }
    }

    rawOpt("parent", "parent") =
      newParentOpt.map(_.patchablePath).map(relativize)
  }

  // 自分を含めた祖先一覧
  def ancestors: List[OutermostRealSpirit] = parentOpt.map { case parent =>
    this :: parent.ancestors
  }.getOrElse { List(this) }

  def withoutParent[T](op: => T): T = {
    val prevFlag = loadParent
    try {
      loadParent = false
      op
    } finally {
      loadParent = prevFlag
    }
  }

  def load() {
    xml = Atmosphere.ioUtils.withPatchedReader(patchablePath)(XML.load)
  }

  def save() {
    val formatted = new PrettyPrinter(80, 2).format(xml)
    Atmosphere.ioUtils.withPatchedWriter(patchablePath)(_.write(formatted))
  }
}

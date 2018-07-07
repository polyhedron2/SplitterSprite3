package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import scala.xml.{Elem, XML, PrettyPrinter}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.common
import jp.gr.java_conf.polyhedron.splittersprite3.outerspace

class ChickenOrEggException(
      spirit: OutermostRealSpirit, newParent: OutermostRealSpirit)
    extends Exception({
  val loopInfo = newParent.ancestors.map(_.patchablePath).mkString(" -> ")
  s"新規の親の親子関係(${loopInfo})は${spirit}とループを起こします。"
})

object OutermostRealSpirit {
  private val body =
    new common.Cache[common.PatchablePath, OutermostRealSpirit] {
      def calc(path: common.PatchablePath) = new OutermostRealSpirit(path)
    }

  def apply(path: common.PatchablePath): OutermostRealSpirit = {
    val ret = body(path)
    try {
      ret.load()
    } catch {
      // ロードがファイル無しで失敗することを許容する
      case e: common.Path.FileIsNotFound =>
    }
    ret
  }

  def apply(pathStr: String): OutermostRealSpirit =
    apply(new common.PatchablePath(pathStr))

  def clear() { body.clear() }
}

// 最も外側のXMLを読み書きするRealSpirit
// １XMLに対してOutermostRealSpiritは１インスタンス
// patchablePath: 内部パス、ファイル区切り文字は'/'で統一
class OutermostRealSpirit private (
    val patchablePath: common.PatchablePath) extends RealSpirit {
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
        pathOpt("special", "parent").map(OutermostRealSpirit(_))
      }
    } else {
      None
    }
  }

  def parentOpt_=(newParentOpt: Option[OutermostRealSpirit]) {
    newParentOpt.foreach { case newParent =>
      val ancestorPaths = newParent.ancestors.map(_.patchablePath).toSet
      if (ancestorPaths(patchablePath)) {
        throw new ChickenOrEggException(this, newParent)
      }
    }

    pathOpt("special", "parent") = newParentOpt.map(_.patchablePath)
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
    xml = patchablePath.withReader(XML.load)

    // scalaのxmlパーザはロード時にattributeを逆転させてしまうので
    // 再ロードして元に戻す
    // 参考：<https://github.com/scala/scala-xml/issues/65>
    xml = XML.loadString(xml.toString)
  }

  def save() {
    val formatted = new PrettyPrinter(80, 2).format(xml)
    patchablePath.withWriter(_.write(formatted))
  }

  override def toString = s"${patchablePath}"
}

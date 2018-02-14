package jp.gr.java_conf.polyhedron.splittersprite3.spirit

// 他XMLの内部XMLを読み書きするRealSpirit
// outer: １つ外側のRealSpirit
// fieldToThis: outerから見たこのInnerRealSpiritの属するfield
class InnerRealSpirit(outer: RealSpirit, fieldToThis: String)
    extends RealSpirit {
  // ロックオブジェクトはOutermostRealSpiritとすることで同一XMLへのアクセスを
  // 管理
  val lock = outer.lock
  val path = s"${outer.path}[${fieldToThis}]"

  def rawValueOpt(field: String) =
    throw new UnsupportedOperationException("TODO: 実装")

  def save() = outer.save()
}

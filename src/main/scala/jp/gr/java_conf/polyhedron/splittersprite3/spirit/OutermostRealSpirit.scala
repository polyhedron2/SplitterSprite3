package jp.gr.java_conf.polyhedron.splittersprite3.spirit

// 最も外側のXMLを読み書きするRealSpirit
// １XMLに対してOutermostRealSpiritは１インスタンス
class OutermostRealSpirit() extends RealSpirit {
  // ロックオブジェクトはOutermostRealSpiritとすることで同一XMLへのアクセスを
  // 管理
  val lock = this
  val path = throw new UnsupportedOperationException("TODO: 実装")

  def rawValueOpt(field: String) =
    throw new UnsupportedOperationException("TODO: 実装")

  def save() = throw new UnsupportedOperationException("TODO: 実装")
}

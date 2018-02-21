package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import jp.gr.java_conf.polyhedron.splittersprite3.common
import jp.gr.java_conf.polyhedron.splittersprite3.agent

// XMLファイルへの読み書きメソッドの呼び出しを記録するためのフェイククラス
class FakeSpirit extends Spirit {
  val internalPath = s"bogus/${this.toString}.xml"
  // 読み書きメソッドの呼び出し時のフィールド名と型を記憶するマップ
  var specMap = Map[String, agent.Spec]()

  val stringOf = new FakeAccessor("this is dummy string.", agent.StringSpec)
  val booleanOf = new FakeAccessor(false, agent.BooleanSpec)
  val intOf = new FakeAccessor(1, agent.IntSpec)
  val doubleOf = new FakeAccessor(1.0, agent.DoubleSpec)

  // 読み書きメソッドの呼び出しをマップに記録するフェイクアクセサ
  // dummyDefaultValue: 読み出しを受けた際のダミー値
  // specFactory: デフォルト値のOptionを受け取り型情報(Specクラスインスタンス)
  //              を返す関数オブジェクト
  class FakeAccessor[VALUE](dummyDefaultValue: VALUE,
                            specFactory: Option[VALUE] => agent.Spec)
      extends Accessor[VALUE] {
    // 書き込まれた値を保持するマップ
    private var dummyValueMap =
      Map[String, VALUE]().withDefaultValue(dummyDefaultValue)
    private def _apply(field: String, defaultOpt: Option[VALUE]) = {
      specMap += (field -> specFactory(defaultOpt))
      dummyValueMap(field)
    }
    def apply(field: String) = _apply(field, None)
    def apply(field: String, default: VALUE) = _apply(field, Some(default))
    def update(field: String, value: VALUE) = dummyValueMap += (field -> value)
  }

  // フェイクのInnerSpirit一覧管理用
  val innerSpiritMap = new common.Cache[String, FakeSpirit] {
    def valueFor(field: String) = new FakeSpirit()
  }
  def apply(field: String) = innerSpiritMap(field)

  def save() { }
}

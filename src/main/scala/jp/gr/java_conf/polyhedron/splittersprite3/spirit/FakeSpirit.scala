package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import scala.reflect.{ClassTag}

import jp.gr.java_conf.polyhedron.splittersprite3.agent
import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.common
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{
  Spawner, OutermostSpawner}

// XMLファイルへの読み書きメソッドの呼び出しを記録するためのフェイククラス
class FakeSpirit() extends Spirit {
  val patchablePath = s"bogus/${this.toString}.xml"
  // 読み書きメソッドの呼び出し時のフィールド名と型を記憶するマップ
  var specMap = Map[String, agent.Specificator.Spec]()

  val string = new FakeValueAccessor("this is dummy string.",
                                     agent.Specificator.StringSpec)
  val boolean = new FakeValueAccessor(false, agent.Specificator.BooleanSpec)
  val int = new FakeValueAccessor(1, agent.Specificator.IntSpec)
  val double = new FakeValueAccessor(1.0, agent.Specificator.DoubleSpec)

  // 読み書きメソッドの呼び出しをマップに記録するフェイクアクセサ
  // dummyDefaultValue: 読み出しを受けた際のダミー値
  // specFactory: デフォルト値のOptionを受け取り型情報(Specクラスインスタンス)
  //              を返す関数オブジェクト
  class FakeValueAccessor[VALUE](
        dummyDefaultValue: VALUE,
        specFactory: Option[VALUE] => agent.Specificator.Spec)
      extends ValueAccessor[VALUE] {
    // 書き込まれた値を保持するマップ
    private var dummyValueMap = Map[String, VALUE]()
    private def apply(field: String, defaultOpt: Option[VALUE]) = {
      specMap += (field -> specFactory(defaultOpt))
      dummyValueMap.get(field).orElse(defaultOpt).getOrElse(dummyDefaultValue)
    }
    def apply(field: String): VALUE = apply(field, None)
    def apply(field: String, default: VALUE): VALUE =
      apply(field, Some(default))
    def update(field: String, value: VALUE): Unit =
      dummyValueMap += (field -> value)
  }

  val outermostSpawner = new OutermostSpawnerAccessor {
    // 書き込まれた値を保持するマップ
    private var dummyValueMap = Map[String, OutermostSpawner[Any]]()

    def apply[T <: OutermostSpawner[Any]: ClassTag](field: String) = {
      val spawnerCls = Atmosphere.reflectionUtils.typeOf[T]
      specMap += (field -> agent.Specificator.OutermostSpawnerSpec(spawnerCls))
      val fakeSpawner =
        Spawner.rawFakeSpawner(spawnerCls).asInstanceOf[OutermostSpawner[Any]]
      dummyValueMap.get(field).getOrElse(fakeSpawner).asInstanceOf[T]
    }

    def update[T <: OutermostSpawner[Any]: ClassTag](field: String, value: T) {
      dummyValueMap += field -> value
    }
  }

  // フェイクのInnerSpirit一覧管理用
  val innerSpiritMap = new common.Cache[String, FakeSpirit] {
    def calc(field: String) = new FakeSpirit()
  }
  def apply(field: String): FakeSpirit = innerSpiritMap(field)

  def save() { }
}

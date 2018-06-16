package jp.gr.java_conf.polyhedron.splittersprite3.spirit

import javafx.scene.image.{Image}
import scala.reflect.{ClassTag}

import jp.gr.java_conf.polyhedron.splittersprite3.agent
import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere, Resources}
import jp.gr.java_conf.polyhedron.splittersprite3.common
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{
  Spawner, OutermostSpawner, InnerSpawner}

// XMLファイルへの読み書きメソッドの呼び出しを記録するためのフェイククラス
class FakeSpirit() extends Spirit {
  val patchablePath = s"bogus/${this.toString}.xml"
  // 読み書きメソッドの呼び出し時のフィールド名と型を記憶するマップ
  var specMap = Map[String, agent.Specificator.Spec]()

  val string = new FakeLiteralAccessor("this is dummy string.",
                                     agent.Specificator.StringSpec)
  val boolean = new FakeLiteralAccessor(false, agent.Specificator.BooleanSpec)
  val int = new FakeLiteralAccessor(1, agent.Specificator.IntSpec)
  val double = new FakeLiteralAccessor(1.0, agent.Specificator.DoubleSpec)

  // 読み書きメソッドの呼び出しをマップに記録するフェイクアクセサ
  // dummyDefaultValue: 読み出しを受けた際のダミー値
  // specFactory: デフォルト値のOptionを受け取り型情報(Specクラスインスタンス)
  //              を返す関数オブジェクト
  class FakeLiteralAccessor[LITERAL](
        dummyDefaultValue: LITERAL,
        specFactory: Option[LITERAL] => agent.Specificator.Spec)
      extends LiteralAccessor[LITERAL] {
    // 書き込まれた値を保持するマップ
    private var dummyValueMap = Map[String, LITERAL]()
    private def apply(field: String, defaultOpt: Option[LITERAL]) = {
      specMap += (field -> specFactory(defaultOpt))
      dummyValueMap.get(field).orElse(defaultOpt).getOrElse(dummyDefaultValue)
    }
    def apply(field: String): LITERAL = apply(field, None)
    def apply(field: String, default: LITERAL): LITERAL =
      apply(field, Some(default))
    def update(field: String, value: LITERAL): Unit =
      dummyValueMap += (field -> value)
  }

  val image = new FakeFileAccessor[Image](Resources.noImage)

  class FakeFileAccessor[FILE_TYPE](value: FILE_TYPE)
      extends FileAccessor[FILE_TYPE] {
    def apply(field: String): FILE_TYPE = value
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

  val innerSpawner = new InnerSpawnerAccessor {
    // 書き込まれた値を保持するマップ
    private var dummyValueMap = Map[String, InnerSpawner[Any]]()

    def apply[T <: InnerSpawner[Any]: ClassTag](field: String) = {
      val spawnerCls = Atmosphere.reflectionUtils.typeOf[T]
      specMap += (field -> agent.Specificator.InnerSpawnerSpec(spawnerCls))
      val fakeSpawner =
        Spawner.rawFakeSpawner(spawnerCls).asInstanceOf[InnerSpawner[Any]]
      dummyValueMap.get(field).getOrElse(fakeSpawner).asInstanceOf[T]
    }

    def update[T <: InnerSpawner[Any]: ClassTag](field: String, value: T) {
      dummyValueMap += field -> value
    }
  }

  def withString: FakeTypeDefiner1[String] =
    new FakeTypeDefiner1[String](agent.Specificator.StringEntrySpec)
  def withOutermostSpawner[
      T1 <: OutermostSpawner[Any]: ClassTag]: FakeTypeDefiner1[T1] = {
    val spawnerCls = Atmosphere.reflectionUtils.typeOf[T1]
    new FakeTypeDefiner1[T1](
      agent.Specificator.OutermostSpawnerEntrySpec(spawnerCls))
  }

  class FakeTypeDefiner1[T1](
        keySpec: agent.Specificator.SimpleEntrySpec
                 with agent.Specificator.VisibleEntrySpec)
      extends FakeTypeDefiner2[String, T1](
        agent.Specificator.InvisibleEntrySpec, keySpec)
      with TypeDefiner1[T1] {

    def andInnerSpawner[T2 <: InnerSpawner[Any]: ClassTag]:
        FakeTypeDefiner2[T1, T2] = {
      val spawnerCls = Atmosphere.reflectionUtils.typeOf[T2]
      new FakeTypeDefiner2[T1, T2](
        keySpec, agent.Specificator.InnerSpawnerEntrySpec(spawnerCls))
    }
  }

  class FakeTypeDefiner2[T1, T2](
        keySpec: agent.Specificator.SimpleEntrySpec,
        valueSpec: agent.Specificator.VisibleEntrySpec)
      extends TypeDefiner2[T1, T2] {
    def kvSeq: FakeKVAccessor[T1, T2] =
      new FakeKVAccessor[T1, T2](keySpec, valueSpec)
  }

  class FakeKVAccessor[T1, T2](
        keySpec: agent.Specificator.SimpleEntrySpec,
        valueSpec: agent.Specificator.VisibleEntrySpec)
      extends KVAccessor[T1, T2] {
    // 書き込まれた値を保持するマップ
    private var dummyValueMap = Map[String, Seq[(T1, T2)]]()

    def apply(field: String): Seq[(T1, T2)] = {
      specMap += (field -> agent.Specificator.KVSpec(keySpec, valueSpec))
      dummyValueMap.get(field).getOrElse(Seq())
    }

    def update(field: String, value: Seq[(T1, T2)]) {
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

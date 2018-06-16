package jp.gr.java_conf.polyhedron.splittersprite3.agent

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.common
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{
  Spawner, OutermostSpawner, InnerSpawner}
import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{Spirit, FakeSpirit}

// SpawnerにFakeSpiritからspawnさせることでフィールド・型情報を
// FakeSpiritに覚えさせるシングルトン
object Specificator
extends common.Cache[Class[_ <: Spawner[Any]], FakeSpirit] {
  override protected def calc(cls: Class[_ <: Spawner[Any]]) = {
    val spirit = new FakeSpirit()

    // クラスインスタンスからリフレクションによりspawnerインスタンスを取得
    val spawner = try {
      // SpawnerにはSpirit１つを引数とするコンストラクタが実装されている
      val constructor = cls.getConstructor(classOf[Spirit])
      constructor.newInstance(spirit)
    } catch {
      case e: Exception => throw new FailureToSpawnFakeInstance(cls, e)
    }

    // lazy valでspiritの値を呼んでいてもFakeSpiritに覚えさせる
    Atmosphere.reflectionUtils.callLazyVals(spawner)

    // spawnを実行
    spawner.spawn(spawner.fakeArgs)
    // フィールド・型情報を覚えたFakeSpiritを返す
    spirit
  }

  // Spiritから読み出された値の型を表現するクラス
  sealed abstract class Spec
  case class StringSpec(defaultOpt: Option[String]) extends Spec
  case class BooleanSpec(defaultOpt: Option[Boolean]) extends Spec
  case class IntSpec(defaultOpt: Option[Int]) extends Spec
  case class DoubleSpec(defaultOpt: Option[Double]) extends Spec
  case class OutermostSpawnerSpec(
    spawnerCls: Class[_ <: OutermostSpawner[Any]]) extends Spec
  case class InnerSpawnerSpec(
    spawnerCls: Class[_ <: InnerSpawner[Any]]) extends Spec
  case class KVSpec(
    keySpec: SimpleEntrySpec, valueSpec: VisibleEntrySpec) extends Spec

  sealed trait EntrySpec
  sealed trait SimpleEntrySpec extends EntrySpec
  sealed trait VisibleEntrySpec extends EntrySpec

  case object InvisibleEntrySpec extends SimpleEntrySpec
  case object StringEntrySpec extends SimpleEntrySpec with VisibleEntrySpec
  case class OutermostSpawnerEntrySpec(
      spawnerCls: Class[_ <: OutermostSpawner[Any]])
    extends SimpleEntrySpec with VisibleEntrySpec
  case class InnerSpawnerEntrySpec(
    spawnerCls: Class[_ <: InnerSpawner[Any]]) extends VisibleEntrySpec

  class FailureToSpawnFakeInstance(
      cls: Class[_ <: Spawner[Any]], cause: Exception)
    extends Exception(s"${cls.getName()}のフェイク生成に失敗しました。", cause)
}

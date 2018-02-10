package splittersprite3.spec

import splittersprite3.common
import splittersprite3.spawner.{Spawner}
import splittersprite3.spirit.{Spirit, FakeSpirit}

class FailureToSpawnFakeInstance(
    cls: Class[_ <: Spawner[Any]], cause: Exception)
  extends Exception(s"${cls.getName()}のフェイク生成に失敗しました。", cause)

// SpawnerにFakeSpiritからspawnさせることでフィールド・型情報を
// FakeSpiritに覚えさせるシングルトン
object Specificator extends common.Cache[Class[_ <: Spawner[Any]],
                                         FakeSpirit] {
  override protected def valueFor(cls: Class[_ <: Spawner[Any]]) = {
    val spirit = new FakeSpirit()

    // クラスインスタンスからリフレクションによりspawnerインスタンスを取得
    val spawner = try {
      val constructor = cls.getConstructor(classOf[Spirit])
      val instance = constructor.newInstance(spirit)
      instance
    } catch {
      case e: Exception => throw new FailureToSpawnFakeInstance(cls, e)
    }

    // spawnを実行
    spawner.fake
    // フィールド・型情報を覚えたFakeSpiritを返す
    spirit
  }
}

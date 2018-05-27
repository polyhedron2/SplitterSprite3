package jp.gr.java_conf.polyhedron.splittersprite3.spawner

import scala.reflect.{ClassTag}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.common
import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{Spirit, FakeSpirit}

class NoConcreteSpawnerClassException(cls: Class[_ <: Spawner[Any]])
  extends Exception(s"${cls.getName()}に具象クラスの実装が必要です。")

// XMLファイルを基にインスタンスを生成するトレイト
sealed trait Spawner[+T] {
  // SpawnerはSpirit１つを引数とするコンストラクタを実装しなければ
  // ならないとする
  val spirit: Spirit

  // 処理中に動的に決定される引数の型定義
  type SpawnArgs
  // インスタンス生成を行うメソッド
  protected def createInstance(args: SpawnArgs): T
  def spawn(args: SpawnArgs): T = {
    createInstance(args)
  }

  // XMLから読み出すフィールド名・型を取り出すためのフェイク実行用の引数
  def fakeArgs: SpawnArgs
}

object Spawner {
  def rawFakeSpawner(spawnerCls: Class[_ <: Spawner[Any]]): Spawner[Any] =
    Atmosphere.reflectionUtils.concreteSubClassList(
        spawnerCls).headOption.getOrElse {
      throw new NoConcreteSpawnerClassException(spawnerCls)
    }.getConstructor(
      classOf[Spirit]).newInstance(new FakeSpirit()).asInstanceOf[Spawner[Any]]

  def fakeSpawner[T <: Spawner[Any]: ClassTag]: T =
    rawFakeSpawner(Atmosphere.reflectionUtils.typeOf[T]).asInstanceOf[T]
}

// OutermostRealSpirit用のSpawner
trait OutermostSpawner[+T] extends Spawner[T]
// InnerRealSpirit用のSpawner
trait InnerSpawner[+T] extends Spawner[T]

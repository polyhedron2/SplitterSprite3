import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.agent
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{
  Spawner, OutermostSpawner, InnerSpawner, NoConcreteSpawnerClassException}
import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{Spirit, FakeSpirit}


trait DummySpawner extends OutermostSpawner[Any] {
  var id: Int = 0
  type SpawnArgs = Unit
  def createInstance(x: Unit) = ()
  val fakeArgs = ()
}
abstract class AbstractSpawner(val spirit: Spirit) extends DummySpawner
class ConcreteSpawner(spirit: Spirit) extends AbstractSpawner(spirit)
class ConcreteSpawnerX(spirit: Spirit) extends ConcreteSpawner(spirit)
class ConcreteSpawnerY(spirit: Spirit) extends ConcreteSpawner(spirit)
class ConcreteSpawnerXX(spirit: Spirit) extends ConcreteSpawnerX(spirit)
class ConcreteSpawnerXY(spirit: Spirit) extends ConcreteSpawnerX(spirit)

abstract class NotImplementedSpawner(val spirit: Spirit) extends DummySpawner

trait DummyInnerSpawner extends InnerSpawner[Any] {
  var id: Int = 0
  type SpawnArgs = Unit
  def createInstance(x: Unit) = ()
  val fakeArgs = ()
}

class ConcreteInnerSpawner(val spirit: Spirit) extends DummyInnerSpawner

class SpawnerSpec extends FlatSpec with DiagrammedAssertions with Matchers {

  "Spawner" should
      "与えられたSpawnerクラスに対し、Spawnerサブクラスインスタンスを返す" in {
    Atmosphere.withTestIOUtils {
      Atmosphere.withTestReflectionUtils(
          classOf[AbstractSpawner], classOf[ConcreteSpawner],
          classOf[ConcreteSpawnerX], classOf[ConcreteSpawnerY],
          classOf[ConcreteSpawnerXX], classOf[ConcreteSpawnerXY]) {
        agent.LoanAgent.loan {
          assert(Spawner.rawFakeSpawner(
            classOf[DummySpawner]).isInstanceOf[DummySpawner])
          assert(Spawner.fakeSpawner[DummySpawner].isInstanceOf[DummySpawner])
          assert(Spawner.rawFakeSpawner(
            classOf[AbstractSpawner]).isInstanceOf[AbstractSpawner])
          assert(Spawner.fakeSpawner[
            AbstractSpawner].isInstanceOf[AbstractSpawner])
          assert(Spawner.rawFakeSpawner(
            classOf[ConcreteSpawner]).isInstanceOf[ConcreteSpawner])
          assert(Spawner.fakeSpawner[
            ConcreteSpawner].isInstanceOf[ConcreteSpawner])
          assert(Spawner.rawFakeSpawner(
            classOf[ConcreteSpawnerX]).isInstanceOf[ConcreteSpawnerX])
          assert(Spawner.fakeSpawner[
            ConcreteSpawnerX].isInstanceOf[ConcreteSpawnerX])
          assert(Spawner.rawFakeSpawner(
            classOf[ConcreteSpawnerY]).isInstanceOf[ConcreteSpawnerY])
          assert(Spawner.fakeSpawner[
            ConcreteSpawnerY].isInstanceOf[ConcreteSpawnerY])
          assert(Spawner.rawFakeSpawner(
            classOf[ConcreteSpawnerXX]).isInstanceOf[ConcreteSpawnerXX])
          assert(Spawner.fakeSpawner[
            ConcreteSpawnerXX].isInstanceOf[ConcreteSpawnerXX])
          assert(Spawner.rawFakeSpawner(
            classOf[ConcreteSpawnerXY]).isInstanceOf[ConcreteSpawnerXY])
          assert(Spawner.fakeSpawner[
            ConcreteSpawnerXY].isInstanceOf[ConcreteSpawnerXY])
        }
      }
    }
  }

  "Spawner" should "具象サブクラスの無いSpawnerのインスタンス取得で例外" in {
    Atmosphere.withTestIOUtils {
      Atmosphere.withTestReflectionUtils(
          classOf[NotImplementedSpawner]) {
        agent.LoanAgent.loan {
          intercept[NoConcreteSpawnerClassException] {
            Spawner.rawFakeSpawner(classOf[NotImplementedSpawner])
          }
          intercept[NoConcreteSpawnerClassException] {
            Spawner.fakeSpawner[NotImplementedSpawner]
          }
        }
      }
    }
  }
}

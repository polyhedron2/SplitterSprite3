import java.nio.file.{Path, Paths, Files}
import scala.reflect.{ClassTag}
import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.agent
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{
  Spawner, OutermostSpawner, InnerSpawner}
import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{Spirit, FakeSpirit}
import jp.gr.java_conf.polyhedron.splittersprite3.outerspace


trait DummySpawner extends OutermostSpawner[Any] {
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

class ReflectionUtilsSpec
    extends FlatSpec with DiagrammedAssertions with Matchers {
  trait SampleTrait
  abstract class SampleAbstractClass
  class SampleConcreteClass

  "ReflectionUtils" should "typeOfで動的な型のクラスオブジェクトを取得" in {
    def func[T: ClassTag] = Atmosphere.reflectionUtils.typeOf[T]

    val intResult = func[Int]
    assert(intResult === classOf[Int])
    val doubleResult = func[Double]
    assert(doubleResult === classOf[Double])

    val traitResult = func[SampleTrait]
    assert(traitResult === classOf[SampleTrait])
    val abstractResult = func[SampleAbstractClass]
    assert(abstractResult === classOf[SampleAbstractClass])
    val concreteResult = func[SampleConcreteClass]
    assert(concreteResult === classOf[SampleConcreteClass])
  }

  "ReflectionUtils" should "無名クラスを判別可能" in {
    assert(Atmosphere.reflectionUtils.isAnonymous(classOf[Int]) === false)
    assert(Atmosphere.reflectionUtils.isAnonymous(classOf[Double]) === false)
    assert(
      Atmosphere.reflectionUtils.isAnonymous(classOf[SampleTrait]) === false)
    assert(
      Atmosphere.reflectionUtils.isAnonymous(classOf[SampleAbstractClass]) ===
      false)
    assert(
      Atmosphere.reflectionUtils.isAnonymous(classOf[SampleConcreteClass]) ===
      false)

    val anonymousClass = new Object() {
      def newMethod() {}
    }.getClass()
    assert(Atmosphere.reflectionUtils.isAnonymous(anonymousClass) === true)
  }

  "ReflectionUtils" should "Spawnerクラスの具象サブクラス一覧を取得可能" in {
    val anonymousSpawnerCls = new AbstractSpawner(new FakeSpirit()) {
    }.getClass()

    Atmosphere.withTestReflectionUtils(Iterator(
        classOf[SampleTrait], classOf[SampleAbstractClass],
        classOf[SampleConcreteClass],
        anonymousSpawnerCls, classOf[AbstractSpawner],
        classOf[ConcreteSpawner],
        classOf[ConcreteSpawnerX], classOf[ConcreteSpawnerY],
        classOf[ConcreteSpawnerXX], classOf[ConcreteSpawnerXY])) {
      Atmosphere.reflectionUtils.concreteSubClassSet(
        classOf[Spawner[Any]]) should be (Set(
        classOf[ConcreteSpawner],
        classOf[ConcreteSpawnerX], classOf[ConcreteSpawnerY],
        classOf[ConcreteSpawnerXX], classOf[ConcreteSpawnerXY]))
      Atmosphere.reflectionUtils.concreteSubClassSet(
        classOf[OutermostSpawner[Any]]) should be (Set(
        classOf[ConcreteSpawner],
        classOf[ConcreteSpawnerX], classOf[ConcreteSpawnerY],
        classOf[ConcreteSpawnerXX], classOf[ConcreteSpawnerXY]))
      Atmosphere.reflectionUtils.concreteSubClassSet(
        classOf[InnerSpawner[Any]]) should be (Set())
      Atmosphere.reflectionUtils.concreteSubClassSet(
        classOf[AbstractSpawner]) should be (Set(
        classOf[ConcreteSpawner],
        classOf[ConcreteSpawnerX], classOf[ConcreteSpawnerY],
        classOf[ConcreteSpawnerXX], classOf[ConcreteSpawnerXY]))
      Atmosphere.reflectionUtils.concreteSubClassSet(
        classOf[ConcreteSpawner]) should be (Set(
        classOf[ConcreteSpawner],
        classOf[ConcreteSpawnerX], classOf[ConcreteSpawnerY],
        classOf[ConcreteSpawnerXX], classOf[ConcreteSpawnerXY]))
      Atmosphere.reflectionUtils.concreteSubClassSet(
        classOf[ConcreteSpawner]) should be (Set(
        classOf[ConcreteSpawner],
        classOf[ConcreteSpawnerX], classOf[ConcreteSpawnerY],
        classOf[ConcreteSpawnerXX], classOf[ConcreteSpawnerXY]))
      Atmosphere.reflectionUtils.concreteSubClassSet(
        classOf[ConcreteSpawnerX]) should be (Set(
        classOf[ConcreteSpawnerX],
        classOf[ConcreteSpawnerXX], classOf[ConcreteSpawnerXY]))
      Atmosphere.reflectionUtils.concreteSubClassSet(
        classOf[ConcreteSpawnerY]) should be (Set(
        classOf[ConcreteSpawnerY]))
      Atmosphere.reflectionUtils.concreteSubClassSet(
        classOf[ConcreteSpawnerXX]) should be (Set(
        classOf[ConcreteSpawnerXX]))
      Atmosphere.reflectionUtils.concreteSubClassSet(
        classOf[ConcreteSpawnerXY]) should be (Set(
        classOf[ConcreteSpawnerXY]))
    }
  }
}

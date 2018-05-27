import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}

import jp.gr.java_conf.polyhedron.splittersprite3.Atmosphere
import jp.gr.java_conf.polyhedron.splittersprite3.agent
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{OutermostSpawner}
import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{Spirit}

object SpecificatorSpec {
  class EmptySpecSpawner(val spirit: Spirit) extends OutermostSpawner[Any] {
    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class ValSpecSpawner(val spirit: Spirit) extends OutermostSpawner[Any] {
    val string = spirit.string("string field")
    val boolean = spirit.boolean("boolean field")
    val int = spirit.int("int field")
    val double = spirit.double("double field")
    val outermostSpawner =
      spirit.outermostSpawner[DummySpawner]("outermost field")

    val innerString = spirit("inner").string("inner string field")
    val innerBoolean = spirit("inner").boolean("inner boolean field")
    val innerInt = spirit("inner").int("inner int field")
    val innerDouble = spirit("inner").double("inner double field")
    val innerOutermostSpawner =
      spirit("inner").outermostSpawner[DummySpawner]("inner outermost field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class LazyValSpecSpawner(val spirit: Spirit) extends OutermostSpawner[Any] {
    lazy val string = spirit.string("string field")
    lazy val boolean = spirit.boolean("boolean field")
    lazy val int = spirit.int("int field")
    lazy val double = spirit.double("double field")
    lazy val outermostSpawner =
      spirit.outermostSpawner[DummySpawner]("outermost field")

    lazy val innerString = spirit("inner").string("inner string field")
    lazy val innerBoolean = spirit("inner").boolean("inner boolean field")
    lazy val innerInt = spirit("inner").int("inner int field")
    lazy val innerDouble = spirit("inner").double("inner double field")
    lazy val innerOutermostSpawner =
      spirit("inner").outermostSpawner[DummySpawner]("inner outermost field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

}

class SpecificatorSpec
    extends FlatSpec with DiagrammedAssertions with Matchers {
  "Specificator" should "Spiritの利用結果を読み込んだFakeSpiritを返す" in {
    Atmosphere.withTestIOUtils {
      Atmosphere.withTestReflectionUtils(
          classOf[SpecificatorSpec.EmptySpecSpawner]) {
        agent.LoanAgent.loan {
          val spirit = agent.Specificator(
            classOf[SpecificatorSpec.EmptySpecSpawner])
          spirit.specMap should be (Map())
          spirit.innerSpiritMap.keys should be (Set())
        }
      }
    }

    Atmosphere.withTestIOUtils {
      Atmosphere.withTestReflectionUtils(
          classOf[SpecificatorSpec.ValSpecSpawner],
          classOf[DummySpawner], classOf[ConcreteSpawner]) {
        agent.LoanAgent.loan {
          val spirit = agent.Specificator(
            classOf[SpecificatorSpec.ValSpecSpawner])
          spirit.specMap should be (Map(
            "string field" -> agent.Specificator.StringSpec(None),
            "boolean field" -> agent.Specificator.BooleanSpec(None),
            "int field" -> agent.Specificator.IntSpec(None),
            "double field" -> agent.Specificator.DoubleSpec(None),
            "outermost field" ->
            agent.Specificator.OutermostSpawnerSpec(classOf[DummySpawner])))
          spirit.innerSpiritMap.keys should be (Set("inner"))
          spirit("inner").specMap should be (Map(
            "inner string field" -> agent.Specificator.StringSpec(None),
            "inner boolean field" -> agent.Specificator.BooleanSpec(None),
            "inner int field" -> agent.Specificator.IntSpec(None),
            "inner double field" -> agent.Specificator.DoubleSpec(None),
            "inner outermost field" ->
            agent.Specificator.OutermostSpawnerSpec(classOf[DummySpawner])))
        }
      }
    }

    Atmosphere.withTestIOUtils {
      Atmosphere.withTestReflectionUtils(
          classOf[SpecificatorSpec.LazyValSpecSpawner],
          classOf[DummySpawner], classOf[ConcreteSpawner]) {
        agent.LoanAgent.loan {
          val spirit = agent.Specificator(
            classOf[SpecificatorSpec.LazyValSpecSpawner])
          spirit.specMap should be (Map(
            "string field" -> agent.Specificator.StringSpec(None),
            "boolean field" -> agent.Specificator.BooleanSpec(None),
            "int field" -> agent.Specificator.IntSpec(None),
            "double field" -> agent.Specificator.DoubleSpec(None),
            "outermost field" ->
            agent.Specificator.OutermostSpawnerSpec(classOf[DummySpawner])))
          spirit.innerSpiritMap.keys should be (Set("inner"))
          spirit("inner").specMap should be (Map(
            "inner string field" -> agent.Specificator.StringSpec(None),
            "inner boolean field" -> agent.Specificator.BooleanSpec(None),
            "inner int field" -> agent.Specificator.IntSpec(None),
            "inner double field" -> agent.Specificator.DoubleSpec(None),
            "inner outermost field" ->
            agent.Specificator.OutermostSpawnerSpec(classOf[DummySpawner])))
        }
      }
    }

  }
}

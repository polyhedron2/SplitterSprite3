import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.agent
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{OutermostSpawner}
import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{Spirit, FakeSpirit}


class FakeSpiritSpec
    extends FlatSpec with DiagrammedAssertions with Matchers {

  def assertSpec(spirit: FakeSpirit) {
    spirit.string("string field 1")
    spirit.string("string field 2", "default")
    spirit.boolean("boolean field 1")
    spirit.boolean("boolean field 2", true)
    spirit.int("integer field 1")
    spirit.int("integer field 2", 42)
    spirit.double("double field 1")
    spirit.double("double field 2", 3.14)
    spirit.outermostSpawner[DummySpawner]("outermost field")
    spirit.innerSpawner[DummyInnerSpawner]("inner field")
    spirit.withString.andInnerSpawner[DummyInnerSpawner].map("map field")
    spirit.withOutermostSpawner[DummySpawner].seq("seq field")
    spirit.withString.set("set field")

    spirit.specMap should be (Map(
      "string field 1" -> agent.Specificator.StringSpec(None),
      "string field 2" -> agent.Specificator.StringSpec(Some("default")),
      "boolean field 1" -> agent.Specificator.BooleanSpec(None),
      "boolean field 2" -> agent.Specificator.BooleanSpec(Some(true)),
      "integer field 1" -> agent.Specificator.IntSpec(None),
      "integer field 2" -> agent.Specificator.IntSpec(Some(42)),
      "double field 1" -> agent.Specificator.DoubleSpec(None),
      "double field 2" -> agent.Specificator.DoubleSpec(Some(3.14)),
      "outermost field" -> agent.Specificator.OutermostSpawnerSpec(
        classOf[DummySpawner]),
      "inner field" -> agent.Specificator.InnerSpawnerSpec(
        classOf[DummyInnerSpawner]),
      "map field" -> agent.Specificator.KVSpec(
        agent.Specificator.StringEntrySpec,
        agent.Specificator.InnerSpawnerEntrySpec(classOf[DummyInnerSpawner])),
      "seq field" -> agent.Specificator.KVSpec(
        agent.Specificator.InvisibleEntrySpec,
        agent.Specificator.OutermostSpawnerEntrySpec(classOf[DummySpawner])),
      "set field" -> agent.Specificator.KVSpec(
        agent.Specificator.InvisibleEntrySpec,
        agent.Specificator.StringEntrySpec)))
  }

  "FakeSpirit" should "呼び出されたフィールドと型を記憶する" in {
    Atmosphere.withTestIOUtils {
      Atmosphere.withTestReflectionUtils(
          classOf[AbstractSpawner], classOf[ConcreteSpawner],
          classOf[ConcreteSpawnerX], classOf[ConcreteSpawnerY],
          classOf[ConcreteSpawnerXX], classOf[ConcreteSpawnerXY],
          classOf[ConcreteInnerSpawner]) {
        agent.LoanAgent.loan {
          val spirit = new FakeSpirit()
          assertSpec(spirit)
          spirit.innerSpiritMap.keys should be (Set())
        }
      }
    }
  }

  "FakeSpirit" should "Inner Spiritも呼び出されたフィールドと型を記憶する" in {
    Atmosphere.withTestIOUtils {
      Atmosphere.withTestReflectionUtils(
          classOf[AbstractSpawner], classOf[ConcreteSpawner],
          classOf[ConcreteSpawnerX], classOf[ConcreteSpawnerY],
          classOf[ConcreteSpawnerXX], classOf[ConcreteSpawnerXY],
          classOf[ConcreteInnerSpawner]) {
        agent.LoanAgent.loan {
          val spirit = new FakeSpirit()
          assertSpec(spirit("inner"))
          spirit.innerSpiritMap.keys should be (Set("inner"))
        }
      }
    }
  }

  def assertUpdate(spirit: FakeSpirit) {
    assert(spirit.string("string field 1") === "this is dummy string.")
    spirit.string("string field 1") = "new string"
    assert(spirit.string("string field 1") === "new string")

    assert(spirit.string("string field 2", "default") === "default")
    spirit.string("string field 2") = "new string 2"
    assert(spirit.string("string field 2", "default") === "new string 2")

    assert(spirit.boolean("boolean field 1") === false)
    spirit.boolean("boolean field 1") = true
    assert(spirit.boolean("boolean field 1") === true)

    assert(spirit.boolean("boolean field 2", true)  === true)
    spirit.boolean("boolean field 2") = false
    assert(spirit.boolean("boolean field 2", true) === false)

    assert(spirit.int("integer field 1") === 1)
    spirit.int("integer field 1") = 2
    assert(spirit.int("integer field 1") === 2)

    assert(spirit.int("integer field 2", 42) === 42)
    spirit.int("integer field 2") = 100
    assert(spirit.int("integer field 2", 42) === 100)

    assert(spirit.double("double field 1") === 1.0)
    spirit.double("double field 1") = 12.5
    assert(spirit.double("double field 1") === 12.5)

    assert(spirit.double("double field 2", 3.14) === 3.14)
    spirit.double("double field 2") = 1024.0
    assert(spirit.double("double field 2", 3.14) === 1024.0)

    assert(spirit.outermostSpawner[DummySpawner]("outermost field").id === 0)
    val newSpawner = new ConcreteSpawner(new FakeSpirit())
    newSpawner.id = 100
    spirit.outermostSpawner("outermost field") = newSpawner
    assert(spirit.outermostSpawner[DummySpawner]("outermost field").id === 100)

    assert(spirit.innerSpawner[DummyInnerSpawner]("inner field").id === 0)
    val newInnerSpawner = new ConcreteInnerSpawner(new FakeSpirit())
    newInnerSpawner.id = 100
    spirit.innerSpawner("inner field") = newInnerSpawner
    assert(spirit.innerSpawner[DummyInnerSpawner]("inner field").id === 100)

    assert(spirit.withString.andInnerSpawner[
      DummyInnerSpawner].kvSeq("kv field") === Seq())
    val newKVSeq = Seq(("key", newInnerSpawner))
    spirit.withString.andInnerSpawner[
      DummyInnerSpawner].kvSeq("kv field") = newKVSeq
    assert(spirit.withString.andInnerSpawner[
      DummyInnerSpawner].kvSeq("kv field") === Seq(("key", newInnerSpawner)))
  }

  "FakeSpirit" should "書き込まれた値を保持する" in {
    Atmosphere.withTestIOUtils {
      Atmosphere.withTestReflectionUtils(
          classOf[AbstractSpawner], classOf[ConcreteSpawner],
          classOf[ConcreteSpawnerX], classOf[ConcreteSpawnerY],
          classOf[ConcreteSpawnerXX], classOf[ConcreteSpawnerXY],
          classOf[ConcreteInnerSpawner]) {
        agent.LoanAgent.loan {
          val spirit = new FakeSpirit()
          assertUpdate(spirit)
        }
      }
    }
  }

  "FakeSpirit" should "inner spiritも書き込まれた値を保持する" in {
    Atmosphere.withTestIOUtils {
      Atmosphere.withTestReflectionUtils(
          classOf[AbstractSpawner], classOf[ConcreteSpawner],
          classOf[ConcreteSpawnerX], classOf[ConcreteSpawnerY],
          classOf[ConcreteSpawnerXX], classOf[ConcreteSpawnerXY],
          classOf[ConcreteInnerSpawner]) {
        agent.LoanAgent.loan {
          val spirit = new FakeSpirit()
          assertUpdate(spirit("inner"))
        }
      }
    }
  }

  "FakeSpirit" should "saveメソッドを持つ" in {
    Atmosphere.withTestIOUtils {
      Atmosphere.withTestReflectionUtils() {
        agent.LoanAgent.loan {
          val spirit = new FakeSpirit()
          spirit.save()
        }
      }
    }
  }
}

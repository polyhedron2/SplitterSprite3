import java.lang.reflect.{InvocationTargetException}
import java.nio.file.{Files}
import scala.xml.{XML, Node}
import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.agent
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{
  OutermostSpawner, InnerSpawner}
import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{
  Spirit, OutermostRealSpirit, SpawnerProcessingLoopException,
  SpawnerIsNotDefined, SpawnerIsInvalid,
  SpiritValueIsNotFound, SpiritValueIsInvalid}

object RealSpiritSpec {
  abstract class BaseSpawner() extends OutermostSpawner[Any]

  class StandardSpawner(val spirit: Spirit) extends BaseSpawner() {
    val string = spirit.string("string field")
    val boolean = spirit.boolean("boolean field")
    val int = spirit.int("int field")
    val double = spirit.double("double field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class DefaultedSpawner(val spirit: Spirit) extends OutermostSpawner[Any] {
    val string = spirit.string("string field", "bar")
    val boolean = spirit.boolean("boolean field", false)
    val int = spirit.int("int field", 123)
    val double = spirit.double("double field", 2.71)

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class InnerValsSpawner(val spirit: Spirit) extends OutermostSpawner[Any] {
    val string = spirit("inner field").string("string field")
    val boolean = spirit("inner field").boolean("boolean field")
    val int = spirit("inner field").int("int field")
    val double = spirit("inner field").double("double field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class DefaultedInnerValsSpawner(val spirit: Spirit)
      extends OutermostSpawner[Any] {
    val string = spirit("inner field").string("string field", "bar")
    val boolean = spirit("inner field").boolean("boolean field", false)
    val int = spirit("inner field").int("int field", 123)
    val double = spirit("inner field").double("double field", 2.71)

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class ReferAnotherSpawner(val spirit: Spirit) extends OutermostSpawner[Any] {
    val anotherSpawner =
      spirit.outermostSpawner[StandardSpawner]("outermost field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class PolymorphismAnotherSpawner(val spirit: Spirit)
      extends OutermostSpawner[Any] {
    val anotherSpawner =
      spirit.outermostSpawner[BaseSpawner]("outermost field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class LazyInfiniteReferSpawner(val spirit: Spirit)
      extends OutermostSpawner[Any] {
    lazy val anotherSpawner =
      spirit.outermostSpawner[LazyInfiniteReferSpawner]("outermost field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class DiligentInfiniteReferSpawner(val spirit: Spirit)
      extends OutermostSpawner[Any] {
    val anotherSpawner =
      spirit.outermostSpawner[DiligentInfiniteReferSpawner]("outermost field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class StandardInnerSpawner(val spirit: Spirit) extends InnerSpawner[Any] {
    val string = spirit.string("string field")
    val boolean = spirit.boolean("boolean field")
    val int = spirit.int("int field")
    val double = spirit.double("double field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class ReferInnerSpawner(val spirit: Spirit) extends OutermostSpawner[Any] {
    val anotherSpawner =
      spirit.innerSpawner[StandardInnerSpawner]("inner field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class MapSpawner(val spirit: Spirit) extends BaseSpawner() {
    val map = spirit.withString.andInnerSpawner[
      StandardInnerSpawner].map("map field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class SeqSpawner(val spirit: Spirit) extends BaseSpawner() {
    val seq = spirit.withOutermostSpawner[StandardSpawner].seq("seq field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class SetSpawner(val spirit: Spirit) extends BaseSpawner() {
    val set = spirit.withString.set("set field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }
}

class RealSpiritSpec extends FlatSpec with DiagrammedAssertions with Matchers {
  def prepare(xmlMap: Map[String, Node]): Map[String, OutermostRealSpirit] = {
    OutermostRealSpirit.clear()
    Files.createDirectories(Atmosphere.ioUtils.verOrPatchDirPath)
    xmlMap.foreach { case (pathStr, xml) =>
      val spiritPath = Atmosphere.ioUtils.verOrPatchDirPath.resolve(pathStr)
      Files.createDirectories(spiritPath.getParent())
      XML.save(spiritPath.toAbsolutePath.toString, xml, "UTF-8")
    }
    xmlMap.map { case (pathStr, xml) =>
      (pathStr, OutermostRealSpirit(pathStr))
    }
  }

  "RealSpirit" should "名称の取得が可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>,
          "testDir/tested_2.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>))

        assert(spiritMap("tested.spirit").name === "tested.spirit")
        assert(spiritMap("testDir/tested_2.spirit").name === "tested_2.spirit")
      }
    }
  }

  "RealSpirit" should "リテラル値の読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>))

        val spawner = spiritMap("tested.spirit").spawner.asInstanceOf[
          RealSpiritSpec.StandardSpawner]
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)
      }
    }
  }

  "RealSpirit" should
      "デフォルト値つきでリテラル値の読み込みが可能" in {
    // 設定値無しではデフォルト値が読み込まれる
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.DefaultedSpawner].getName()
            }</spawner>
          </root>))

        val spawner = spiritMap("tested.spirit").spawner.asInstanceOf[
          RealSpiritSpec.DefaultedSpawner]
        assert(spawner.string === "bar")
        assert(spawner.boolean === false)
        assert(spawner.int === 123)
        assert(spawner.double === 2.71)
      }
    }

    // 設定値有りではXMLの値が読み込まれる
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.DefaultedSpawner].getName()
            }</spawner>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>))

        val spawner = spiritMap("tested.spirit").spawner.asInstanceOf[
          RealSpiritSpec.DefaultedSpawner]
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)
      }
    }
  }

  "RealSpirit" should "inner spiritでもリテラル値の読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.InnerValsSpawner].getName()
            }</spawner>
            <inner field="inner field">
              <string field="string field">foo</string>
              <boolean field="boolean field">true</boolean>
              <int field="int field">42</int>
              <double field="double field">3.14</double>
            </inner>
          </root>))

        val spawner = spiritMap("tested.spirit").spawner.asInstanceOf[
          RealSpiritSpec.InnerValsSpawner]
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)
      }
    }
  }

  "RealSpirit" should
      "inner spiritでもデフォルト値つきでリテラル値の読み込みが可能" in {
    // 設定値無しではデフォルト値が読み込まれる
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.DefaultedInnerValsSpawner].getName()
            }</spawner>
          </root>))

        val spawner = spiritMap("tested.spirit").spawner.asInstanceOf[
          RealSpiritSpec.DefaultedInnerValsSpawner]
        assert(spawner.string === "bar")
        assert(spawner.boolean === false)
        assert(spawner.int === 123)
        assert(spawner.double === 2.71)
      }
    }

    // 設定値有りではXMLの値が読み込まれる
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.DefaultedInnerValsSpawner].getName()
            }</spawner>
            <inner field="inner field">
              <string field="string field">foo</string>
              <boolean field="boolean field">true</boolean>
              <int field="int field">42</int>
              <double field="double field">3.14</double>
            </inner>
          </root>))

        val spawner = spiritMap("tested.spirit").spawner.asInstanceOf[
          RealSpiritSpec.DefaultedInnerValsSpawner]
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)
      }
    }
  }

  "RealSpirit" should "他OutermostSpawnerの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.ReferAnotherSpawner].getName()
            }</spawner>
            <outermost field="outermost field">referred.spirit</outermost>
          </root>,
          "referred.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>))

        val spawner = spiritMap("tested.spirit").spawner.asInstanceOf[
          RealSpiritSpec.ReferAnotherSpawner]
        assert(spawner.anotherSpawner.string === "foo")
        assert(spawner.anotherSpawner.boolean === true)
        assert(spawner.anotherSpawner.int === 42)
        assert(spawner.anotherSpawner.double === 3.14)
      }
    }
  }

  "RealSpirit" should "他OutermostSpawnerはサブクラス指定でも読み込み可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.PolymorphismAnotherSpawner].getName()
            }</spawner>
            <outermost field="outermost field">referred.spirit</outermost>
          </root>,
          "referred.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>))

        val spawner = spiritMap("tested.spirit").spawner.asInstanceOf[
          RealSpiritSpec.PolymorphismAnotherSpawner]
        assert(
          spawner.anotherSpawner.isInstanceOf[RealSpiritSpec.StandardSpawner])
      }
    }
  }

  "RealSpirit" should
      "他OutermostSpawnerは遅延評価なら同一クラス指定でも読み込み可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.LazyInfiniteReferSpawner].getName()
            }</spawner>
            <outermost field="outermost field">tested.spirit</outermost>
          </root>))

        val spawner = spiritMap("tested.spirit").spawner.asInstanceOf[
          RealSpiritSpec.LazyInfiniteReferSpawner]
        assert(spawner.anotherSpawner.isInstanceOf[
          RealSpiritSpec.LazyInfiniteReferSpawner])
      }
    }
  }

  "RealSpirit" should
      "他OutermostSpawnerは遅延評価無しなら同一クラス指定で無限ループ例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.DiligentInfiniteReferSpawner].getName()
            }</spawner>
            <outermost field="outermost field">tested.spirit</outermost>
          </root>))

        val e = intercept[InvocationTargetException] {
          spiritMap("tested.spirit").spawner
        }
        assert(e.getCause().isInstanceOf[SpiritValueIsInvalid])
        assert(
          e.getCause().getCause().isInstanceOf[SpawnerProcessingLoopException])
      }
    }
  }

  "RealSpirit" should "未設定値により例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
          </root>))

        val e = intercept[InvocationTargetException] {
          spiritMap("tested.spirit").spawner
        }
        assert(e.getCause().isInstanceOf[SpiritValueIsNotFound])
      }
    }

    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.ReferAnotherSpawner].getName()
            }</spawner>
          </root>))

        val e = intercept[InvocationTargetException] {
          spiritMap("tested.spirit").spawner
        }
        assert(e.getCause().isInstanceOf[SpiritValueIsNotFound])
      }
    }

    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map("tested.spirit" -> <root></root>))

        intercept[SpawnerIsNotDefined] {
          spiritMap("tested.spirit").spawner
        }
      }
    }
  }

  "RealSpirit" should "不正な設定値により例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">foo</string>
            <boolean field="boolean field">yes</boolean>
          </root>))

        val e = intercept[InvocationTargetException] {
          spiritMap("tested.spirit").spawner
        }
        assert(e.getCause().isInstanceOf[SpiritValueIsInvalid])
      }
    }

    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.ReferAnotherSpawner].getName()
            }</spawner>
            <outermost field="outermost field">nope.spirit</outermost>
          </root>))

        val e = intercept[InvocationTargetException] {
          spiritMap("tested.spirit").spawner
        }
        assert(e.getCause().isInstanceOf[SpiritValueIsInvalid])
      }
    }

    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">invalid cls path</spawner>
          </root>))

        intercept[SpawnerIsInvalid] {
          spiritMap("tested.spirit").spawner
        }
      }
    }
  }

  "RealSpirit" should "InnerSpawnerの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.ReferInnerSpawner].getName()
            }</spawner>
            <inner field="inner field">
              <spawner field="spawner">{
                classOf[RealSpiritSpec.StandardInnerSpawner].getName()
              }</spawner>
              <string field="string field">foo</string>
              <boolean field="boolean field">true</boolean>
              <int field="int field">42</int>
              <double field="double field">3.14</double>
            </inner>
          </root>))

        val spawner = spiritMap("tested.spirit").spawner.asInstanceOf[
          RealSpiritSpec.ReferInnerSpawner]
        assert(spawner.anotherSpawner.isInstanceOf[
          RealSpiritSpec.StandardInnerSpawner])
        assert(spawner.anotherSpawner.string === "foo")
        assert(spawner.anotherSpawner.boolean === true)
        assert(spawner.anotherSpawner.int === 42)
        assert(spawner.anotherSpawner.double === 3.14)
      }
    }
  }

  "RealSpirit" should "InnerSpawnerの不正な設定値により例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.ReferInnerSpawner].getName()
            }</spawner>
            <inner field="inner field">
              <spawner field="spawner">invalid spawner cls path</spawner>
              <string field="string field">foo</string>
              <boolean field="boolean field">true</boolean>
              <int field="int field">42</int>
              <double field="double field">3.14</double>
            </inner>
          </root>))

        val e = intercept[InvocationTargetException] {
          spiritMap("tested.spirit").spawner
        }
        assert(e.getCause().isInstanceOf[SpiritValueIsInvalid])
      }
    }
  }

  "RealSpirit" should "Mapを読み込み可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.MapSpawner].getName()
            }</spawner>
            <inner field="map field">
              <inner field="hoge">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">string_hoge</string>
                <boolean field="boolean field">false</boolean>
                <int field="int field">1000</int>
                <double field="double field">1000.0</double>
              </inner>
              <inner field="fuga">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">string_fuga</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">2000</int>
                <double field="double field">2000.0</double>
              </inner>
            </inner>
          </root>))

        val spawner = spiritMap("tested.spirit").spawner.asInstanceOf[
          RealSpiritSpec.MapSpawner]
        spawner.map.keySet should be (Set("hoge", "fuga"))
        assert(spawner.map("hoge").string === "string_hoge")
        assert(spawner.map("hoge").boolean === false)
        assert(spawner.map("hoge").int === 1000)
        assert(spawner.map("hoge").double === 1000.0)
        assert(spawner.map("fuga").string === "string_fuga")
        assert(spawner.map("fuga").boolean === true)
        assert(spawner.map("fuga").int === 2000)
        assert(spawner.map("fuga").double === 2000.0)
      }
    }
  }

  "RealSpirit" should "Seqを読み込み可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.SeqSpawner].getName()
            }</spawner>
            <inner field="seq field">
              <!--フィールド名が順序が大きい順になっている-->
              <outermost field="001">referred_2.spirit</outermost>
              <outermost field="000">referred_1.spirit</outermost>
            </inner>
          </root>,
          "referred_1.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>,
          "referred_2.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">bar</string>
            <boolean field="boolean field">false</boolean>
            <int field="int field">24</int>
            <double field="double field">2.71</double>
          </root>))

        val spawner = spiritMap("tested.spirit").spawner.asInstanceOf[
          RealSpiritSpec.SeqSpawner]
        assert(spawner.seq.length === 2)
        assert(spawner.seq(0).string === "foo")
        assert(spawner.seq(0).boolean === true)
        assert(spawner.seq(0).int === 42)
        assert(spawner.seq(0).double === 3.14)
        assert(spawner.seq(1).string === "bar")
        assert(spawner.seq(1).boolean === false)
        assert(spawner.seq(1).int === 24)
        assert(spawner.seq(1).double === 2.71)
      }
    }
  }
  "RealSpirit" should "Setを読み込み可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.SetSpawner].getName()
            }</spawner>
            <inner field="set field">
              <string field="000">first</string>
              <string field="001">second</string>
              <string field="002">first</string>
            </inner>
          </root>))

        val spawner = spiritMap("tested.spirit").spawner.asInstanceOf[
          RealSpiritSpec.SetSpawner]
        assert(spawner.set === Set("first", "second"))
      }
    }
  }
}

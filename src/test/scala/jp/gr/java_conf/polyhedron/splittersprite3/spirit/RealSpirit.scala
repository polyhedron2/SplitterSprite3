import java.lang.reflect.{InvocationTargetException}
import java.nio.file.{Files}
import scala.xml.{XML, Node, PrettyPrinter}
import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.agent
import jp.gr.java_conf.polyhedron.splittersprite3.common
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{
  OutermostSpawner, InnerSpawner}
import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{
  Spirit, OutermostRealSpirit, ChickenOrEggException,
  SpawnerProcessingLoopException, SpawnerIsNotDefined,
  SpawnerNotHaveSpiritConstructor, SpawnerIsInvalid, SpiritValueIsNotFound,
  SpiritValueIsInvalid}

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

  class StandardInnerSpawner(val spirit: Spirit) extends InnerSpawner[Any] {
    val string = spirit.string("string field")
    val boolean = spirit.boolean("boolean field")
    val int = spirit.int("int field")
    val double = spirit.double("double field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class InfiniteLoopSpawner(val spirit: Spirit) extends BaseSpawner() {
    spirit.outermostSpawner[InfiniteLoopSpawner]("outermost field")

    type SpawnArgs = Unit
    def createInstance(x: Unit) = ()
    val fakeArgs = ()
  }

  class InvalidConstructorSpawner(
      val spirit: Spirit, i: Int) extends BaseSpawner() {
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
          "tested.spirit" -> <root></root>,
          "testDir/tested_2.spirit" -> <root></root>))

        assert(spiritMap("tested.spirit").name === "tested.spirit")
        assert(spiritMap("testDir/tested_2.spirit").name === "tested_2.spirit")
      }
    }
  }

  "RealSpirit" should "ファイル保存が可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        prepare(Map())

        val saveSpirit = OutermostRealSpirit("tested.spirit", requireFile=false)
        saveSpirit.string("hoge") = "foo"
        saveSpirit.int("fuga") = 42
        saveSpirit.save()

        OutermostRealSpirit.clear()

        val loadSpirit = OutermostRealSpirit("tested.spirit")
        assert(loadSpirit.string("hoge") === "foo")
        assert(loadSpirit.int("fuga") === 42)
      }
    }
  }

  "RealSpirit" should "inner spiritからでもファイル保存が可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        prepare(Map())

        val saveSpirit = OutermostRealSpirit("tested.spirit", requireFile=false)
        saveSpirit.string("hoge") = "foo"
        saveSpirit.int("fuga") = 42
        saveSpirit("inner").save()

        OutermostRealSpirit.clear()

        val loadSpirit = OutermostRealSpirit("tested.spirit")
        assert(loadSpirit.string("hoge") === "foo")
        assert(loadSpirit.int("fuga") === 42)
      }
    }
  }

  "RealSpirit" should "親スピリットの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <parent field="parent">parent.spirit</parent>
          </root>,
          "parent.spirit" -> <root/>))
        assert(spiritMap("tested.spirit").parentOpt ===
               Some(spiritMap("parent.spirit")))
      }

      agent.LoanAgent.loan {
        val spiritMap = prepare(Map("tested.spirit" -> <root/>))
        assert(spiritMap("tested.spirit").parentOpt === None)
      }
    }
  }

  "RealSpirit" should "親スピリットの書き込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root/>,
          "parent.spirit" -> <root/>))

        spiritMap("tested.spirit").parentOpt = Some(spiritMap("parent.spirit"))
        assert(spiritMap("tested.spirit").parentOpt ===
               Some(spiritMap("parent.spirit")))

        val formatted =
          new PrettyPrinter(80, 2).format(spiritMap("tested.spirit").xml)
        assert(
          formatted === List(
          """<root>""",
          """  <parent field="parent">parent.spirit</parent>""",
          """</root>""").mkString("\n"))
      }
    }
  }

  "RealSpirit" should "ループする親スピリットの書き込みで例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root/>,
          "parent.spirit" -> <root/>))

        spiritMap("tested.spirit").parentOpt = Some(spiritMap("parent.spirit"))
        assert(spiritMap("tested.spirit").parentOpt ===
               Some(spiritMap("parent.spirit")))

        intercept[ChickenOrEggException] {
          spiritMap("parent.spirit").parentOpt =
            Some(spiritMap("tested.spirit"))
        }
      }
    }
  }


  "RealSpirit" should "Spawnerクラスの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
          </root>))
        assert(spiritMap("tested.spirit").spawnerClassOpt ===
               Some(classOf[RealSpiritSpec.StandardSpawner]))
      }

      agent.LoanAgent.loan {
        val spiritMap = prepare(Map("tested.spirit" -> <root/>))
        assert(spiritMap("tested.spirit").spawnerClassOpt === None)
      }
    }
  }

  "RealSpirit" should "不正なSpawner名で例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">invalid cls path</spawner>
          </root>))

        intercept[SpawnerIsInvalid] {
          spiritMap("tested.spirit").spawnerClassOpt
        }
      }
    }
  }

  "RealSpirit" should "Spawnerの無限ループで例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.InfiniteLoopSpawner].getName()
            }</spawner>
            <outermost field="outermost field">tested.spirit</outermost>
          </root>))

        val e = intercept[InvocationTargetException] {
          spiritMap("tested.spirit").spawner
        }
        e.getCause().isInstanceOf[SpawnerProcessingLoopException]
      }
    }
  }

  "RealSpirit" should "Spawner名未定義で例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map("tested.spirit" -> <root/>))

        intercept[SpawnerIsNotDefined] {
          spiritMap("tested.spirit").spawner
        }
      }
    }
  }

  "RealSpirit" should "不正なSpawnerのコンストラクタ引数で例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.InvalidConstructorSpawner].getName()
            }</spawner>
          </root>))

        intercept[SpawnerNotHaveSpiritConstructor] {
          spiritMap("tested.spirit").spawner
        }
      }
    }
  }

  "RealSpirit" should "親スピリットからSpawnerクラスの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <parent field="parent">parent.spirit</parent>
          </root>,
          "parent.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
          </root>))
        assert(spiritMap("tested.spirit").spawnerClassOpt ===
               Some(classOf[RealSpiritSpec.StandardSpawner]))
      }
    }
  }

  "RealSpirit" should "Spawnerクラスの書き込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map("tested.spirit" -> <root/>))
        spiritMap("tested.spirit").spawnerClassOpt =
          Some(classOf[RealSpiritSpec.StandardSpawner])

        assert(spiritMap("tested.spirit").spawnerClassOpt ===
               Some(classOf[RealSpiritSpec.StandardSpawner]))

        val formatted =
          new PrettyPrinter(80, 2).format(spiritMap("tested.spirit").xml)
        assert(
          formatted === List(
          """<root>""",
          """  <spawner field="spawner">""" +
          classOf[RealSpiritSpec.StandardSpawner].getName() +
          """</spawner>""",
          """</root>""").mkString("\n"))
      }
    }
  }

  "RealSpirit" should "リテラル値の読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>))

        assert(spiritMap("tested.spirit").string("string field") === "foo")
        assert(spiritMap("tested.spirit").boolean("boolean field") === true)
        assert(spiritMap("tested.spirit").int("int field") === 42)
        assert(spiritMap("tested.spirit").double("double field") === 3.14)
      }
    }
  }

  "RealSpirit" should "親スピリットからもリテラル値の読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <parent field="parent">parent.spirit</parent>
          </root>,
          "parent.spirit" -> <root>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>))

        assert(spiritMap("tested.spirit").string("string field") === "foo")
        assert(spiritMap("tested.spirit").boolean("boolean field") === true)
        assert(spiritMap("tested.spirit").int("int field") === 42)
        assert(spiritMap("tested.spirit").double("double field") === 3.14)
      }
    }
  }

  "RealSpirit" should "リテラル値の書き込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map("tested.spirit" -> <root/>))

        spiritMap("tested.spirit").string("string field") = "foo"
        spiritMap("tested.spirit").boolean("boolean field") = true
        spiritMap("tested.spirit").int("int field") = 42
        spiritMap("tested.spirit").double("double field") = 3.14

        assert(spiritMap("tested.spirit").string("string field") === "foo")
        assert(spiritMap("tested.spirit").boolean("boolean field") === true)
        assert(spiritMap("tested.spirit").int("int field") === 42)
        assert(spiritMap("tested.spirit").double("double field") === 3.14)

        val formatted =
          new PrettyPrinter(80, 2).format(spiritMap("tested.spirit").xml)
        assert(
          formatted === List(
          """<root>""",
          """  <boolean field="boolean field">true</boolean>""",
          """  <double field="double field">3.14</double>""",
          """  <int field="int field">42</int>""",
          """  <string field="string field">foo</string>""",
          """</root>""").mkString("\n"))
      }
    }
  }

  "RealSpirit" should "inner spiritでもリテラル値の読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <inner field="inner field">
              <string field="string field">foo</string>
              <boolean field="boolean field">true</boolean>
              <int field="int field">42</int>
              <double field="double field">3.14</double>
            </inner>
          </root>))

        assert(spiritMap(
          "tested.spirit")("inner field").string("string field") === "foo")
        assert(spiritMap(
          "tested.spirit")("inner field").boolean("boolean field") === true)
        assert(spiritMap(
          "tested.spirit")("inner field").int("int field") === 42)
        assert(spiritMap(
          "tested.spirit")("inner field").double("double field") === 3.14)
      }
    }
  }

  "RealSpirit" should
      "親スピリットからでもinner spiritでもリテラル値の読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <parent field="parent">parent.spirit</parent>
          </root>,
          "parent.spirit" -> <root>
            <inner field="inner field">
              <string field="string field">foo</string>
              <boolean field="boolean field">true</boolean>
              <int field="int field">42</int>
              <double field="double field">3.14</double>
            </inner>
          </root>))

        assert(spiritMap(
          "tested.spirit")("inner field").string("string field") === "foo")
        assert(spiritMap(
          "tested.spirit")("inner field").boolean("boolean field") === true)
        assert(spiritMap(
          "tested.spirit")("inner field").int("int field") === 42)
        assert(spiritMap(
          "tested.spirit")("inner field").double("double field") === 3.14)
      }
    }
  }

  "RealSpirit" should "inner spiritでもリテラル値の書き込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map("tested.spirit" -> <root/>))

        spiritMap(
          "tested.spirit")("inner field").string("string field") = "foo"
        spiritMap(
          "tested.spirit")("inner field").boolean("boolean field") = true
        spiritMap(
          "tested.spirit")("inner field").int("int field") = 42
        spiritMap(
          "tested.spirit")("inner field").double("double field") = 3.14

        assert(spiritMap(
          "tested.spirit")("inner field").string("string field") === "foo")
        assert(spiritMap(
          "tested.spirit")("inner field").boolean("boolean field") === true)
        assert(spiritMap(
          "tested.spirit")("inner field").int("int field") === 42)
        assert(spiritMap(
          "tested.spirit")("inner field").double("double field") === 3.14)

        val formatted =
          new PrettyPrinter(80, 2).format(spiritMap("tested.spirit").xml)
        assert(
          formatted === List(
          """<root>""",
          """  <inner field="inner field">""",
          """    <boolean field="boolean field">true</boolean>""",
          """    <double field="double field">3.14</double>""",
          """    <int field="int field">42</int>""",
          """    <string field="string field">foo</string>""",
          """  </inner>""",
          """</root>""").mkString("\n"))
      }
    }
  }

  "RealSpirit" should "デフォルト値つきでリテラル値の読み込みが可能" in {
    // 設定値無しではデフォルト値が読み込まれる
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map("tested.spirit" -> <root/>))

        assert(spiritMap(
          "tested.spirit").string("string field", "bar") === "bar")
        assert(spiritMap(
          "tested.spirit").boolean("boolean field", false) === false)
        assert(spiritMap(
          "tested.spirit").int("int field", 256) === 256)
        assert(spiritMap(
          "tested.spirit").double("double field", 2.71) === 2.71)
      }
    }

    // 設定値有りではXMLの値が読み込まれる
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>))

        assert(spiritMap(
          "tested.spirit").string("string field", "bar") === "foo")
        assert(spiritMap(
          "tested.spirit").boolean("boolean field", false) === true)
        assert(spiritMap(
          "tested.spirit").int("int field", 256) === 42)
        assert(spiritMap(
          "tested.spirit").double("double field", 2.71) === 3.14)
      }
    }
  }

  "RealSpirit" should "不正なリテラル値で例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <boolean field="boolean field">yes</boolean>
            <int field="int field">infinity</int>
            <double field="double field">1 + 2i</double>
          </root>))

        intercept[SpiritValueIsInvalid] {
          spiritMap("tested.spirit").boolean("boolean field")
        }
        intercept[SpiritValueIsInvalid] {
          spiritMap("tested.spirit").int("int field")
        }
        intercept[SpiritValueIsInvalid] {
          spiritMap("tested.spirit").double("double field")
        }
      }
    }
  }

  "RealSpirit" should "未設定のリテラル値で例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map("tested.spirit" -> <root/>))

        intercept[SpiritValueIsNotFound] {
          spiritMap("tested.spirit").string("string field")
        }
        intercept[SpiritValueIsNotFound] {
          spiritMap("tested.spirit").boolean("boolean field")
        }
        intercept[SpiritValueIsNotFound] {
          spiritMap("tested.spirit").int("int field")
        }
        intercept[SpiritValueIsNotFound] {
          spiritMap("tested.spirit").double("double field")
        }
      }
    }
  }

  "RealSpirit" should "他OutermostSpawnerの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "firstDir/tested.spirit" -> <root>
            <outermost field="outermost field">{
              "../secondDir/referred.spirit"
            }</outermost>
          </root>,
          "secondDir/referred.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>))

        val spawner = spiritMap("firstDir/tested.spirit")
          .outermostSpawner[RealSpiritSpec.StandardSpawner]("outermost field")
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)
      }
    }
  }

  "RealSpirit" should "他OutermostSpawnerの書き込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "firstDir/tested.spirit" -> <root/>,
          "secondDir/referred.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>))

        spiritMap(
            "firstDir/tested.spirit").outermostSpawner("outermost field") =
          spiritMap("secondDir/referred.spirit").spawner.asInstanceOf[
            RealSpiritSpec.StandardSpawner]

       val spawner = spiritMap("firstDir/tested.spirit")
         .outermostSpawner[RealSpiritSpec.StandardSpawner]("outermost field")
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)

       val formatted = new PrettyPrinter(80, 2).format(
          spiritMap("firstDir/tested.spirit").xml)
        assert(
          formatted === List(
          """<root>""",
          """  <outermost field="outermost field">""" +
          """../secondDir/referred.spirit""" +
          """</outermost>""",
          """</root>""").mkString("\n"))
       }
    }
  }

  "RealSpirit" should "inner spiritでも他OutermostSpawnerの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "firstDir/tested.spirit" -> <root>
            <inner field="inner field">
              <outermost field="outermost field">{
                "../secondDir/referred.spirit"
              }</outermost>
            </inner>
          </root>,
          "secondDir/referred.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>))

        val spawner = spiritMap("firstDir/tested.spirit")("inner field")
          .outermostSpawner[RealSpiritSpec.StandardSpawner]("outermost field")
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)
      }
    }
  }

  "RealSpirit" should "inner spiritでも他OutermostSpawnerの書き込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "firstDir/tested.spirit" -> <root/>,
          "secondDir/referred.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>))

        spiritMap("firstDir/tested.spirit")("inner field")
            .outermostSpawner("outermost field") =
          spiritMap("secondDir/referred.spirit").spawner.asInstanceOf[
            RealSpiritSpec.StandardSpawner]

       val spawner = spiritMap("firstDir/tested.spirit")("inner field")
         .outermostSpawner[RealSpiritSpec.StandardSpawner]("outermost field")
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)

       val formatted = new PrettyPrinter(80, 2).format(
          spiritMap("firstDir/tested.spirit").xml)
        assert(
          formatted === List(
          """<root>""",
          """  <inner field="inner field">""",
          """    <outermost field="outermost field">""" +
          """../secondDir/referred.spirit""" +
          """</outermost>""",
          """  </inner>""",
          """</root>""").mkString("\n"))
       }
    }
  }

  "RealSpirit" should "不正なOutermostSpawnerで例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <outermost field="outermost field">dummy.spirit</outermost>
          </root>))

        intercept[SpiritValueIsInvalid] {
          spiritMap("tested.spirit").outermostSpawner("outermost field")
        }
      }
    }
  }

  "RealSpirit" should "未設定のOutermostSpawnerで例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map("tested.spirit" -> <root/>))

        intercept[SpiritValueIsNotFound] {
          spiritMap("tested.spirit").outermostSpawner("outermost field")
        }
      }
    }
  }

  "RealSpirit" should "InnerSpawnerの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
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

        val spawner = spiritMap("tested.spirit").innerSpawner[
          RealSpiritSpec.StandardInnerSpawner]("inner field")

        assert(spawner.isInstanceOf[RealSpiritSpec.StandardInnerSpawner])
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)
      }
    }
  }

  "RealSpirit" should "親スピリットからでもInnerSpawnerの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <parent field="parent">parent.spirit</parent>
          </root>,
          "parent.spirit" -> <root>
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

        val spawner = spiritMap("tested.spirit").innerSpawner[
          RealSpiritSpec.StandardInnerSpawner]("inner field")

        assert(spawner.isInstanceOf[RealSpiritSpec.StandardInnerSpawner])
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)
      }
    }
  }

  "RealSpirit" should "InnerSpawnerの書き込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root/>,
          "another.spirit" ->
          <root>
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

        spiritMap("tested.spirit").innerSpawner("inner field") =
          spiritMap("another.spirit").innerSpawner[
            RealSpiritSpec.StandardInnerSpawner]("inner field")

        val spawner = spiritMap("tested.spirit").innerSpawner[
          RealSpiritSpec.StandardInnerSpawner]("inner field")

        assert(spawner.isInstanceOf[RealSpiritSpec.StandardInnerSpawner])
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)

        val formatted = new PrettyPrinter(80, 2).format(
          spiritMap("tested.spirit").xml)
        assert(
          formatted === List(
          """<root>""",
          """  <inner field="inner field">""",
          """    <spawner field="spawner">""" +
          classOf[RealSpiritSpec.StandardInnerSpawner].getName() +
          """</spawner>""",
          """    <string field="string field">foo</string>""",
          """    <boolean field="boolean field">true</boolean>""",
          """    <int field="int field">42</int>""",
          """    <double field="double field">3.14</double>""",
          """  </inner>""",
          """</root>""").mkString("\n"))
      }
    }
  }

  "RealSpirit" should "inner spiritでもInnerSpawnerの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <inner field="inner field">
              <inner field="inner field 2">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">foo</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">42</int>
                <double field="double field">3.14</double>
              </inner>
            </inner>
          </root>))

        val spawner = spiritMap("tested.spirit")("inner field").innerSpawner[
          RealSpiritSpec.StandardInnerSpawner]("inner field 2")

        assert(spawner.isInstanceOf[RealSpiritSpec.StandardInnerSpawner])
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)
      }
    }
  }

  "RealSpirit" should
      "親スピリットからでもinner spiritでもInnerSpawnerの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <parent field="parent">parent.spirit</parent>
          </root>,
          "parent.spirit" -> <root>
            <inner field="inner field">
              <inner field="inner field 2">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">foo</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">42</int>
                <double field="double field">3.14</double>
              </inner>
            </inner>
          </root>))

        val spawner = spiritMap("tested.spirit")("inner field").innerSpawner[
          RealSpiritSpec.StandardInnerSpawner]("inner field 2")

        assert(spawner.isInstanceOf[RealSpiritSpec.StandardInnerSpawner])
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)
      }
    }
  }

  "RealSpirit" should "inner spiritでもInnerSpawnerの書き込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root/>,
          "another.spirit" ->
          <root>
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

        spiritMap("tested.spirit")("inner field")
            .innerSpawner("inner field 2") =
          spiritMap("another.spirit").innerSpawner[
            RealSpiritSpec.StandardInnerSpawner]("inner field")

        val spawner = spiritMap("tested.spirit")("inner field").innerSpawner[
          RealSpiritSpec.StandardInnerSpawner]("inner field 2")

        assert(spawner.isInstanceOf[RealSpiritSpec.StandardInnerSpawner])
        assert(spawner.string === "foo")
        assert(spawner.boolean === true)
        assert(spawner.int === 42)
        assert(spawner.double === 3.14)

        val formatted = new PrettyPrinter(80, 2).format(
          spiritMap("tested.spirit").xml)
        assert(
          formatted === List(
          """<root>""",
          """  <inner field="inner field">""",
          """    <inner field="inner field 2">""",
          """      <spawner field="spawner">""" +
          classOf[RealSpiritSpec.StandardInnerSpawner].getName() +
          """</spawner>""",
          """      <string field="string field">foo</string>""",
          """      <boolean field="boolean field">true</boolean>""",
          """      <int field="int field">42</int>""",
          """      <double field="double field">3.14</double>""",
          """    </inner>""",
          """  </inner>""",
          """</root>""").mkString("\n"))
      }
    }
  }

  "RealSpirit" should "kvSeqの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <inner field="kv field">
              <!--- ("fuga", "hoge", "piyo") を ("hoge", "fuga", "piyo") に -->
              <permutation field="permutation">hoge \\ fuga</permutation>
              <inner field="hoge">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">foo</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">42</int>
                <double field="double field">3.14</double>
              </inner>
              <inner field="fuga">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">bar</string>
                <boolean field="boolean field">false</boolean>
                <int field="int field">420</int>
                <double field="double field">31.4</double>
              </inner>
              <inner field="piyo">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">baz</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">4200</int>
                <double field="double field">314</double>
              </inner>
            </inner>
          </root>))

        val kvSeq = spiritMap("tested.spirit")
          .withString.andInnerSpawner[RealSpiritSpec.StandardInnerSpawner]
          .kvSeq("kv field")
        assert(kvSeq.size === 3)
        kvSeq.map(_._1) should be (Seq("hoge", "fuga", "piyo"))
        assert(kvSeq(0)._2.string === "foo")
        assert(kvSeq(1)._2.string === "bar")
        assert(kvSeq(2)._2.string === "baz")
        assert(kvSeq(0)._2.boolean === true)
        assert(kvSeq(1)._2.boolean === false)
        assert(kvSeq(2)._2.boolean === true)
        assert(kvSeq(0)._2.int === 42)
        assert(kvSeq(1)._2.int === 420)
        assert(kvSeq(2)._2.int === 4200)
        assert(kvSeq(0)._2.double === 3.14)
        assert(kvSeq(1)._2.double === 31.4)
        assert(kvSeq(2)._2.double === 314)
      }
    }
  }

  "RealSpirit" should "親スピリット組み合わせてもkvSeqの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <parent field="parent">parent.spirit</parent>
            <inner field="kv field">
              <!--- ("fuga", "hoge", "piyo") を ("hoge", "fuga", "piyo") に -->
              <permutation field="permutation">hoge \\ fuga</permutation>
              <inner field="hoge">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">foo</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">42</int>
                <double field="double field">3.14</double>
              </inner>
              <inner field="hogera" deleted="true"/>
            </inner>
          </root>,
          "parent.spirit" -> <root>
            <inner field="kv field">
              <inner field="fuga">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">bar</string>
                <boolean field="boolean field">false</boolean>
                <int field="int field">420</int>
                <double field="double field">31.4</double>
              </inner>
              <inner field="piyo">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">baz</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">4200</int>
                <double field="double field">314</double>
              </inner>
              <inner field="hogera">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">qux</string>
                <boolean field="boolean field">false</boolean>
                <int field="int field">42000</int>
                <double field="double field">3140</double>
              </inner>
            </inner>
          </root>))

        val kvSeq = spiritMap("tested.spirit")
          .withString.andInnerSpawner[RealSpiritSpec.StandardInnerSpawner]
          .kvSeq("kv field")
        assert(kvSeq.size === 3)
        kvSeq.map(_._1) should be (Seq("hoge", "fuga", "piyo"))
        assert(kvSeq(0)._2.string === "foo")
        assert(kvSeq(1)._2.string === "bar")
        assert(kvSeq(2)._2.string === "baz")
        assert(kvSeq(0)._2.boolean === true)
        assert(kvSeq(1)._2.boolean === false)
        assert(kvSeq(2)._2.boolean === true)
        assert(kvSeq(0)._2.int === 42)
        assert(kvSeq(1)._2.int === 420)
        assert(kvSeq(2)._2.int === 4200)
        assert(kvSeq(0)._2.double === 3.14)
        assert(kvSeq(1)._2.double === 31.4)
        assert(kvSeq(2)._2.double === 314)
      }
    }
  }

  "RealSpirit" should "kvSeqの書き込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root/>,
          "another.spirit" -> <root>
            <inner field="kv field 2">
              <inner field="hoge 2">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">foo</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">42</int>
                <double field="double field">3.14</double>
              </inner>
              <inner field="fuga 2">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">bar</string>
                <boolean field="boolean field">false</boolean>
                <int field="int field">420</int>
                <double field="double field">31.4</double>
              </inner>
              <inner field="piyo 2">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">baz</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">4200</int>
                <double field="double field">314</double>
              </inner>
            </inner>
          </root>))

        val anotherKVMap = spiritMap("another.spirit")
          .withString.andInnerSpawner[RealSpiritSpec.StandardInnerSpawner]
          .map("kv field 2")

        // piyo, fuga, hogeの順で書き込む
        spiritMap("tested.spirit")
            .withString.andInnerSpawner[RealSpiritSpec.StandardInnerSpawner]
            .kvSeq("kv field") =
          Seq(
            "piyo" -> anotherKVMap("piyo 2"),
            "fuga" -> anotherKVMap("fuga 2"),
            "hoge" -> anotherKVMap("hoge 2"))

        val kvSeq = spiritMap("tested.spirit")
          .withString.andInnerSpawner[RealSpiritSpec.StandardInnerSpawner]
          .kvSeq("kv field")
        assert(kvSeq.size === 3)
        kvSeq.map(_._1) should be (Seq("piyo", "fuga", "hoge"))
        assert(kvSeq(0)._2.string === "baz")
        assert(kvSeq(1)._2.string === "bar")
        assert(kvSeq(2)._2.string === "foo")
        assert(kvSeq(0)._2.boolean === true)
        assert(kvSeq(1)._2.boolean === false)
        assert(kvSeq(2)._2.boolean === true)
        assert(kvSeq(0)._2.int === 4200)
        assert(kvSeq(1)._2.int === 420)
        assert(kvSeq(2)._2.int === 42)
        assert(kvSeq(0)._2.double === 314)
        assert(kvSeq(1)._2.double === 31.4)
        assert(kvSeq(2)._2.double === 3.14)

        val formatted = new PrettyPrinter(80, 2).format(
          spiritMap("tested.spirit").xml)
        assert(
          formatted === List(
          """<root>""",
          """  <inner field="kv field">""",
          """    <permutation field="permutation">fuga \\ hoge \\ piyo""" +
          """</permutation>""",
          """    <inner field="fuga">""",
          """      <spawner field="spawner">""" +
          classOf[RealSpiritSpec.StandardInnerSpawner].getName() +
          """</spawner>""",
          """      <string field="string field">bar</string>""",
          """      <boolean field="boolean field">false</boolean>""",
          """      <int field="int field">420</int>""",
          """      <double field="double field">31.4</double>""",
          """    </inner>""",
          """    <inner field="hoge">""",
          """      <spawner field="spawner">""" +
          classOf[RealSpiritSpec.StandardInnerSpawner].getName() +
          """</spawner>""",
          """      <string field="string field">foo</string>""",
          """      <boolean field="boolean field">true</boolean>""",
          """      <int field="int field">42</int>""",
          """      <double field="double field">3.14</double>""",
          """    </inner>""",
          """    <inner field="piyo">""",
          """      <spawner field="spawner">""" +
          classOf[RealSpiritSpec.StandardInnerSpawner].getName() +
          """</spawner>""",
          """      <string field="string field">baz</string>""",
          """      <boolean field="boolean field">true</boolean>""",
          """      <int field="int field">4200</int>""",
          """      <double field="double field">314</double>""",
          """    </inner>""",
          """  </inner>""",
          """</root>""").mkString("\n"))
      }
    }
  }

  "RealSpirit" should "親スピリットを組み合わせてもkvSeqの書き込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <parent field="parent">parent.spirit</parent>
          </root>,
          "parent.spirit" -> <root>
            <inner field="kv field">
              <inner field="hogera">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">qux</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">42000</int>
                <double field="double field">3140</double>
              </inner>
            </inner>
          </root>,
          "another.spirit" -> <root>
            <inner field="kv field 2">
              <inner field="hoge 2">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">foo</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">42</int>
                <double field="double field">3.14</double>
              </inner>
              <inner field="fuga 2">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">bar</string>
                <boolean field="boolean field">false</boolean>
                <int field="int field">420</int>
                <double field="double field">31.4</double>
              </inner>
              <inner field="piyo 2">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">baz</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">4200</int>
                <double field="double field">314</double>
              </inner>
            </inner>
          </root>))

        val anotherKVMap = spiritMap("another.spirit")
          .withString.andInnerSpawner[RealSpiritSpec.StandardInnerSpawner]
          .map("kv field 2")

        // piyo, fuga, hogeの順で書き込む
        spiritMap("tested.spirit")
            .withString.andInnerSpawner[RealSpiritSpec.StandardInnerSpawner]
            .kvSeq("kv field") =
          Seq(
            "piyo" -> anotherKVMap("piyo 2"),
            "fuga" -> anotherKVMap("fuga 2"),
            "hoge" -> anotherKVMap("hoge 2"))

        val kvSeq = spiritMap("tested.spirit")
          .withString.andInnerSpawner[RealSpiritSpec.StandardInnerSpawner]
          .kvSeq("kv field")
        assert(kvSeq.size === 3)
        kvSeq.map(_._1) should be (Seq("piyo", "fuga", "hoge"))
        assert(kvSeq(0)._2.string === "baz")
        assert(kvSeq(1)._2.string === "bar")
        assert(kvSeq(2)._2.string === "foo")
        assert(kvSeq(0)._2.boolean === true)
        assert(kvSeq(1)._2.boolean === false)
        assert(kvSeq(2)._2.boolean === true)
        assert(kvSeq(0)._2.int === 4200)
        assert(kvSeq(1)._2.int === 420)
        assert(kvSeq(2)._2.int === 42)
        assert(kvSeq(0)._2.double === 314)
        assert(kvSeq(1)._2.double === 31.4)
        assert(kvSeq(2)._2.double === 3.14)

        val formatted = new PrettyPrinter(80, 2).format(
          spiritMap("tested.spirit").xml)
        assert(
          formatted === List(
          """<root>""",
          """  <inner field="kv field">""",
          """    <permutation field="permutation">fuga \\ hoge \\ piyo""" +
          """</permutation>""",
          """    <inner field="fuga">""",
          """      <spawner field="spawner">""" +
          classOf[RealSpiritSpec.StandardInnerSpawner].getName() +
          """</spawner>""",
          """      <string field="string field">bar</string>""",
          """      <boolean field="boolean field">false</boolean>""",
          """      <int field="int field">420</int>""",
          """      <double field="double field">31.4</double>""",
          """    </inner>""",
          """    <inner field="hoge">""",
          """      <spawner field="spawner">""" +
          classOf[RealSpiritSpec.StandardInnerSpawner].getName() +
          """</spawner>""",
          """      <string field="string field">foo</string>""",
          """      <boolean field="boolean field">true</boolean>""",
          """      <int field="int field">42</int>""",
          """      <double field="double field">3.14</double>""",
          """    </inner>""",
          """    <inner deleted="true" field="hogera"/>""",
          """    <inner field="piyo">""",
          """      <spawner field="spawner">""" +
          classOf[RealSpiritSpec.StandardInnerSpawner].getName() +
          """</spawner>""",
          """      <string field="string field">baz</string>""",
          """      <boolean field="boolean field">true</boolean>""",
          """      <int field="int field">4200</int>""",
          """      <double field="double field">314</double>""",
          """    </inner>""",
          """  </inner>""",
          """  <parent field="parent">parent.spirit</parent>""",
          """</root>""").mkString("\n"))
      }
    }
  }

  "RealSpirit" should "mapの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <inner field="kv field">
              <!--- ("fuga", "hoge", "piyo") を ("hoge", "fuga", "piyo") に -->
              <permutation field="permutation">hoge \\ fuga</permutation>
              <inner field="hoge">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">foo</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">42</int>
                <double field="double field">3.14</double>
              </inner>
              <inner field="fuga">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">bar</string>
                <boolean field="boolean field">false</boolean>
                <int field="int field">420</int>
                <double field="double field">31.4</double>
              </inner>
              <inner field="piyo">
                <spawner field="spawner">{
                  classOf[RealSpiritSpec.StandardInnerSpawner].getName()
                }</spawner>
                <string field="string field">baz</string>
                <boolean field="boolean field">true</boolean>
                <int field="int field">4200</int>
                <double field="double field">314</double>
              </inner>
            </inner>
          </root>))

        val map = spiritMap("tested.spirit")
          .withString.andInnerSpawner[RealSpiritSpec.StandardInnerSpawner]
          .map("kv field")
        assert(map.size === 3)
        map.keySet should be (Set("hoge", "fuga", "piyo"))
        assert(map("hoge").string === "foo")
        assert(map("fuga").string === "bar")
        assert(map("piyo").string === "baz")
        assert(map("hoge").boolean === true)
        assert(map("fuga").boolean === false)
        assert(map("piyo").boolean === true)
        assert(map("hoge").int === 42)
        assert(map("fuga").int === 420)
        assert(map("piyo").int === 4200)
        assert(map("hoge").double === 3.14)
        assert(map("fuga").double === 31.4)
        assert(map("piyo").double === 314)
      }
    }
  }

  "RealSpirit" should "seqの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "firstDir/tested.spirit" -> <root>
            <inner field="kv field">
              <!--- ("fuga", "hoge", "piyo") を ("hoge", "fuga", "piyo") に -->
              <permutation field="permutation">hoge \\ fuga</permutation>
              <outermost field="hoge">../secondDir/referred.spirit</outermost>
              <outermost field="fuga">../thirdDir/referred.spirit</outermost>
              <outermost field="piyo">../fourthDir/referred.spirit</outermost>
            </inner>
          </root>,
          "secondDir/referred.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">foo</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">42</int>
            <double field="double field">3.14</double>
          </root>,
          "thirdDir/referred.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">bar</string>
            <boolean field="boolean field">true</boolean>
            <int field="int field">420</int>
            <double field="double field">31.4</double>
          </root>,
          "fourthDir/referred.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="string field">baz</string>
            <boolean field="boolean field">false</boolean>
            <int field="int field">4200</int>
            <double field="double field">314</double>
          </root>))

        val seq = spiritMap("firstDir/tested.spirit")
          .withOutermostSpawner[RealSpiritSpec.StandardSpawner].seq("kv field")
        assert(seq.size === 3)
        assert(seq(0).string === "foo")
        assert(seq(1).string === "bar")
        assert(seq(2).string === "baz")
        assert(seq(0).boolean === true)
        assert(seq(1).boolean === true)
        assert(seq(2).boolean === false)
        assert(seq(0).int === 42)
        assert(seq(1).int === 420)
        assert(seq(2).int === 4200)
        assert(seq(0).double === 3.14)
        assert(seq(1).double === 31.4)
        assert(seq(2).double === 314)
      }
    }
  }

  "RealSpirit" should "setの読み込みが可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <inner field="kv field">
              <string field="hoge">foo</string>
              <string field="fuga">bar</string>
              <string field="piyo">baz</string>
            </inner>
          </root>))

        val set = spiritMap("tested.spirit").withString.set("kv field")
        set should be (Set("foo", "bar", "baz"))
      }
    }
  }

  "RealSpirit" should "親スピリットのXMLを合成可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <parent field="parent">parent.spirit</parent>
            <string field="child only">hoge</string>
            <string field="child and parent">fuga</string>
            <inner field="inner field">
              <string field="child only">hoge</string>
              <string field="child and parent">fuga</string>
            </inner>
          </root>,
          "parent.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="parent only">foo</string>
            <string field="child and parent">bar</string>
            <inner field="inner field">
              <string field="parent only">foo</string>
              <string field="child and parent">bar</string>
            </inner>
          </root>))

        val formatted = new PrettyPrinter(80, 2).format(
          spiritMap("tested.spirit").compositeXML)

        assert(
          formatted === List(
          """<root>""",
          """  <inner field="inner field">""",
          """    <string field="child and parent">fuga</string>""",
          """    <string field="child only">hoge</string>""",
          """    <string field="parent only">foo</string>""",
          """  </inner>""",
          """  <parent field="parent">parent.spirit</parent>""",
          """  <spawner field="spawner">""" +
          classOf[RealSpiritSpec.StandardSpawner].getName() +
          """</spawner>""",
          """  <string field="child and parent">fuga</string>""",
          """  <string field="child only">hoge</string>""",
          """  <string field="parent only">foo</string>""",
          """</root>""").mkString("\n"))

        val formattedInner = new PrettyPrinter(80, 2).format(
          spiritMap("tested.spirit")("inner field").compositeXML)

        assert(
          formattedInner === List(
          """<inner field="inner field">""",
          """  <string field="child and parent">fuga</string>""",
          """  <string field="child only">hoge</string>""",
          """  <string field="parent only">foo</string>""",
          """</inner>""").mkString("\n"))
      }
    }
  }

  "RealSpirit" should "親スピリットの影響を切り替え可能" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        val spiritMap = prepare(Map(
          "tested.spirit" -> <root>
            <parent field="parent">parent.spirit</parent>
            <string field="child only">hoge</string>
            <string field="child and parent">fuga</string>
            <inner field="inner field">
              <string field="child only">hoge</string>
              <string field="child and parent">fuga</string>
            </inner>
          </root>,
          "parent.spirit" -> <root>
            <spawner field="spawner">{
              classOf[RealSpiritSpec.StandardSpawner].getName()
            }</spawner>
            <string field="parent only">foo</string>
            <string field="child and parent">bar</string>
            <inner field="inner field">
              <string field="parent only">foo</string>
              <string field="child and parent">bar</string>
            </inner>
          </root>))

        val formatted = new PrettyPrinter(80, 2).format(
          spiritMap("tested.spirit").withoutParent {
            spiritMap("tested.spirit").compositeXML
          })

        assert(
          formatted === List(
          """<root>""",
          """  <parent field="parent">parent.spirit</parent>""",
          """  <string field="child only">hoge</string>""",
          """  <string field="child and parent">fuga</string>""",
          """  <inner field="inner field">""",
          """    <string field="child only">hoge</string>""",
          """    <string field="child and parent">fuga</string>""",
          """  </inner>""",
          """</root>""").mkString("\n"))

        val formattedInner = new PrettyPrinter(80, 2).format(
          spiritMap("tested.spirit")("inner field").withoutParent {
            spiritMap("tested.spirit")("inner field").compositeXML
          })

        assert(
          formattedInner === List(
          """<inner field="inner field">""",
          """  <string field="child only">hoge</string>""",
          """  <string field="child and parent">fuga</string>""",
          """</inner>""").mkString("\n"))
      }
    }
  }
}

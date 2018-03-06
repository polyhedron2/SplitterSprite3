import org.scalatest.{FlatSpec, DiagrammedAssertions}

import jp.gr.java_conf.polyhedron.splittersprite3.common

class CacheSpec extends FlatSpec with DiagrammedAssertions {
    //文字列をDoubleに変換するサンプル
    val strDoubleCache = new common.Cache[String, Double]() {
        def valueFor(key: String) = {
            key.toDouble
        }
    }

    //サンプルオブジェクト用クラス
    case class Value(id: Int)

    //文字列をDoubleに変換するサンプル
    val intObjCache = new common.Cache[Int, Value]() {
        def valueFor(key: Int) = {
            Value(key)
        }
    }

    "Cache" should "valueForで定義された値を返す" in {
        assert(strDoubleCache("0") === 0)
        assert(strDoubleCache("0.0") === 0)
        assert(strDoubleCache("1.0") === 1)
        assert(strDoubleCache("42") === 42)
        assert(strDoubleCache("0.5") === 0.5)

        assert(intObjCache(0) === Value(0))
        assert(intObjCache(1) === Value(1))
        assert(intObjCache(100) === Value(100))
    }

    "Cache" should "valueForの例外はそのまま返す" in {
        intercept[NumberFormatException] { strDoubleCache("") }
        intercept[NumberFormatException] { strDoubleCache("foo") }
    }

    var callCount = Map[String, Int]().withDefaultValue(0)
    //呼び出し回数カウント
    val countCache = new common.Cache[String, Double]() {
        def valueFor(key: String) = {
            val prevCount = callCount(key)
            callCount += (key -> (prevCount + 1))
            key.toDouble
        }
    }

    "Cache" should "valueForの中身は２回呼ばれない" in {
        def assertCallCount(key: String) {
            assert(callCount(key) === 0)
            countCache(key)
            assert(callCount(key) === 1)
            countCache(key)
            assert(callCount(key) === 1)
        }

        assertCallCount("0")
        assertCallCount("1")
        assertCallCount("42")
        assertCallCount("0.5")
    }

    //valueForが無限ループを起こすサンプル
    lazy val infiniteLoopCache: common.Cache[String, Value] =
        new common.Cache[String, Value]() {
            def valueFor(key: String) = {
                infiniteLoopCache(key)
            }
        }

    "Cache" should "valueForの無限ループは例外を返す" in {
        intercept[common.InfiniteLoopCacheException] { infiniteLoopCache("") }
    }

    "Cache" should "valueForが例外終了したらprocessingフラグは戻る" in {
        intercept[NumberFormatException] { strDoubleCache("") }
        // InfiniteLoopCacheExceptionではなくNumberFormatExceptionが返る
        intercept[NumberFormatException] { strDoubleCache("") }
    }
}

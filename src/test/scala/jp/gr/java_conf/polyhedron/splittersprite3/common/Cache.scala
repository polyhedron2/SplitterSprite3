import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}

import jp.gr.java_conf.polyhedron.splittersprite3.common

class CacheSpec extends FlatSpec with DiagrammedAssertions with Matchers {
    //文字列をDoubleに変換するサンプル
    val strDoubleCache = new common.Cache[String, Double]() {
        def calc(key: String) = {
            key.toDouble
        }
    }

    //サンプルオブジェクト用クラス
    case class Value(id: Int)

    //文字列をDoubleに変換するサンプル
    val intObjCache = new common.Cache[Int, Value]() {
        def calc(key: Int) = {
            Value(key)
        }
    }

    "Cache" should "calcで定義された値を返す" in {
        assert(strDoubleCache("0") === 0)
        assert(strDoubleCache("0.0") === 0)
        assert(strDoubleCache("1.0") === 1)
        assert(strDoubleCache("42") === 42)
        assert(strDoubleCache("0.5") === 0.5)

        assert(intObjCache(0) === Value(0))
        assert(intObjCache(1) === Value(1))
        assert(intObjCache(100) === Value(100))
    }

    "Cache" should "保持ているキーの一覧を返す" in {
        strDoubleCache.keys.toSet should be (
            Set("0", "0.0", "1.0", "42", "0.5"))
        intObjCache.keys.toSet should be (Set(0, 1, 100))
    }

    "Cache" should "calcの例外はそのまま返す" in {
        intercept[NumberFormatException] { strDoubleCache("") }
        intercept[NumberFormatException] { strDoubleCache("foo") }
    }

    "Cache" should "外部から与えられた値があればそれを保持" in {
        // 一度キャッシュをクリア
        strDoubleCache.clear()
        intObjCache.clear()
        strDoubleCache.keys.toSet should be (Set())
        intObjCache.keys.toSet should be (Set())

        strDoubleCache("0.0") = 42
        assert(strDoubleCache("0.0") === 42)
        intObjCache(0) = Value(42)
        assert(intObjCache(0) === Value(42))

        // 一度値がキャッシュされていても上書き可能
        assert(strDoubleCache("1.0") === 1)
        assert(intObjCache(1) === Value(1))
        strDoubleCache("1.0") = 42
        assert(strDoubleCache("1.0") === 42)
        intObjCache(1) = Value(42)
        assert(intObjCache(1) === Value(42))
    }

    var callCount = Map[String, Int]().withDefaultValue(0)
    //呼び出し回数カウント
    val countCache = new common.Cache[String, Double]() {
        def calc(key: String) = {
            val prevCount = callCount(key)
            callCount += (key -> (prevCount + 1))
            key.toDouble
        }
    }

    "Cache" should "calcの中身は２回呼ばれない" in {
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

    //calcが無限ループを起こすサンプル
    lazy val infiniteLoopCache: common.Cache[String, Value] =
        new common.Cache[String, Value]() {
            def calc(key: String) = {
                infiniteLoopCache(key)
            }
        }

    "Cache" should "calcの無限ループは例外を返す" in {
        intercept[common.InfiniteLoopCacheException] { infiniteLoopCache("") }
    }

    "Cache" should "calcが例外終了したらprocessingフラグは戻る" in {
        intercept[NumberFormatException] { strDoubleCache("") }
        // InfiniteLoopCacheExceptionではなくNumberFormatExceptionが返る
        intercept[NumberFormatException] { strDoubleCache("") }
    }
}

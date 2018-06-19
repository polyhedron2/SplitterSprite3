import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}

import jp.gr.java_conf.polyhedron.splittersprite3.common

class PermutationSpec
    extends FlatSpec with DiagrammedAssertions with Matchers {

  "Permutation" should "全単射にならない定義で例外" in {
    intercept[common.InvalidPermutation] {
      new common.Permutation(Map(1 -> 2))
    }
    intercept[common.InvalidPermutation] {
      new common.Permutation(Map(1 -> 2, 2 -> 3, 3 -> 2))
    }
    intercept[common.InvalidPermutation] {
      new common.Permutation(Map("hoge" -> "fuga"))
    }
  }

  "CyclePermutation" should "ループになっていない定義で例外" in {
    intercept[common.InvalidPermutation] {
      new common.CyclePermutation(Seq(1, 2, 3, 2, 4))
    }
    intercept[common.InvalidPermutation] {
      new common.CyclePermutation(Seq(1, 2, 1))
    }
    intercept[common.InvalidPermutation] {
      new common.CyclePermutation(Seq(1, 1))
    }
    intercept[common.InvalidPermutation] {
      new common.CyclePermutation(Seq("hoge", "fuga", "hoge"))
    }
  }

  "Permutation" should "同一置換を判定可能" in {
    assert(
      new common.Permutation(Map()) === new common.Permutation(Map(1 -> 1)))
    assert(
      new common.Permutation(Map("hoge" -> "hoge")) ===
      new common.Permutation(Map(1 -> 1)))
    assert(
      new common.Permutation(Map(1 -> 2, 2 -> 3, 3 -> 1)) ===
      new common.Permutation(Map(3 -> 1, 2 -> 3, 1 -> 2, 10 -> 10)))

    assert(
      new common.Permutation(Map()) !==
      new common.Permutation(Map(1 -> 2, 2 -> 1)))
    assert(
      new common.Permutation(Map("hoge" -> "hoge")) !==
        new common.Permutation(Map(1 -> 2, 2 -> 1)))
    assert(
      new common.Permutation(Map(1 -> 2, 2 -> 3, 3 -> 1)) !==
      new common.Permutation(Map(3 -> 1, 2 -> 3, 1 -> 2, 10 -> 11, 11 -> 10)))

    assert(
      new common.Permutation(Map(1 -> 2, 2 -> 3, 3 -> 1)) !== 1)
    assert(
      new common.Permutation(Map(1 -> 2, 2 -> 3, 3 -> 1)) !== "hoge")
    assert(
      new common.Permutation(Map(1 -> 2, 2 -> 3, 3 -> 1)) !==
      Map(1 -> 2, 2 -> 3, 3 -> 1))
  }

  "Permutation" should "入れ替え対象一覧を返す" in {
    new common.Permutation(Map()).domain should be (Set())
    new common.Permutation(Map(1 -> 2, 2 -> 1)).domain should be (Set(1, 2))
    new common.Permutation(Map(
      1 -> 2, 2 -> 1, 3 -> 3)).domain should be (Set(1, 2))
    new common.Permutation(
      Map("hoge" -> "fuga", "fuga" -> "hoge")).domain should be (
        Set("hoge", "fuga"))
  }

  "Permutation" should "入れ替えを行う" in {
    assert(new common.Permutation(Map(1 -> 2, 2 -> 1))(0) === 0)
    assert(new common.Permutation(Map(1 -> 2, 2 -> 1))(1) === 2)
    assert(new common.Permutation(Map(1 -> 2, 2 -> 1))(2) === 1)
    assert(new common.Permutation(Map(1 -> 2, 2 -> 1))(3) === 3)
    assert(new common.Permutation(Map(1 -> 2, 2 -> 1))(4) === 4)

    assert(new common.Permutation(
      Map("hoge" -> "fuga", "fuga" -> "hoge"))("hoge") === "fuga")
    assert(new common.Permutation(
      Map("hoge" -> "fuga", "fuga" -> "hoge"))("fuga") === "hoge")
    assert(new common.Permutation(
      Map("hoge" -> "fuga", "fuga" -> "hoge"))("piyo") === "piyo")

    // 無関係な型でも処理可能
    assert(new common.Permutation(Map(1 -> 2, 2 -> 1))("hoge") === "hoge")

    assert(new common.CyclePermutation(Seq(1, 2, 3))(0) === 0)
    assert(new common.CyclePermutation(Seq(1, 2, 3))(1) === 2)
    assert(new common.CyclePermutation(Seq(1, 2, 3))(2) === 3)
    assert(new common.CyclePermutation(Seq(1, 2, 3))(3) === 1)
    assert(new common.CyclePermutation(Seq(1, 2, 3))(4) === 4)
  }

  "Permutation" should "逆向きの入れ替えを行う" in {
    assert(new common.Permutation(Map(1 -> 2, 2 -> 1)).inverse(0) === 0)
    assert(new common.Permutation(Map(1 -> 2, 2 -> 1)).inverse(2) === 1)
    assert(new common.Permutation(Map(1 -> 2, 2 -> 1)).inverse(1) === 2)
    assert(new common.Permutation(Map(1 -> 2, 2 -> 1)).inverse(3) === 3)
    assert(new common.Permutation(Map(1 -> 2, 2 -> 1)).inverse(4) === 4)

    assert(new common.Permutation(
      Map("hoge" -> "fuga", "fuga" -> "hoge")).inverse("fuga") === "hoge")
    assert(new common.Permutation(
      Map("hoge" -> "fuga", "fuga" -> "hoge")).inverse("hoge") === "fuga")
    assert(new common.Permutation(
      Map("hoge" -> "fuga", "fuga" -> "hoge")).inverse("piyo") === "piyo")

    // 無関係な型でも処理可能
    assert(new common.Permutation(
      Map(1 -> 2, 2 -> 1)).inverse("hoge") === "hoge")

    assert(new common.CyclePermutation(Seq(1, 2, 3)).inverse(0) === 0)
    assert(new common.CyclePermutation(Seq(1, 2, 3)).inverse(2) === 1)
    assert(new common.CyclePermutation(Seq(1, 2, 3)).inverse(3) === 2)
    assert(new common.CyclePermutation(Seq(1, 2, 3)).inverse(1) === 3)
    assert(new common.CyclePermutation(Seq(1, 2, 3)).inverse(4) === 4)
  }

  "Permutation" should "合成可能" in {
    val p1 = new common.CyclePermutation(Seq(1, 2))
    val p2 = new common.CyclePermutation(Seq(2, 3))

    def assertComposite(target: Int) {
      assert((p1 * p2)(target) === p1(p2(target)))
      assert((p1 *: p2)(target) === p1(p2(target)))
    }

    assertComposite(0)
    assertComposite(1)
    assertComposite(2)
    assertComposite(3)
    assertComposite(4)
  }

  "Permutation" should "合成の実行順序は自由" in {
    val p1 = new common.CyclePermutation(Seq(1, 2))
    val p2 = new common.CyclePermutation(Seq(2, 3))
    val p3 = new common.CyclePermutation(Seq(3, 4))

    def assertAssociativity(target: Int) {
      val expected = (p1 *: p2 *: p3)(target)
      assert(expected === (p1 *: (p2 *: p3))(target))
      assert(expected === ((p1 *: p2) *: p3)(target))
    }

    assertAssociativity(0)
    assertAssociativity(1)
    assertAssociativity(2)
    assertAssociativity(3)
    assertAssociativity(4)
    assertAssociativity(5)
  }

  "Permutation" should "順序定義を入れ替える" in {
    val order = (a: Int, b: Int) => a < b

    (0 until 10).sortWith(
      order *: new common.CyclePermutation(Seq(1, 2))) should be (
        Seq(0, 2, 1, 3, 4, 5, 6, 7, 8, 9))
    (0 until 10).sortWith(
      order *: new common.CyclePermutation(Seq(1, 2, 3))) should be (
        Seq(0, 3, 1, 2, 4, 5, 6, 7, 8, 9))
    (0 until 10).sortWith(order *: new common.Permutation(
      Map(1 -> 2, 2 -> 3, 3 -> 1, 8 -> 9, 9 -> 8))) should be (
        Seq(0, 3, 1, 2, 4, 5, 6, 7, 9, 8))
  }

  "Permutation" should "順序定義の入れ替え順序は自由" in {
    val order = (a: Int, b: Int) => a < b
    val p1 = new common.CyclePermutation(Seq(1, 2))
    val p2 = new common.CyclePermutation(Seq(2, 3))
    val p3 = new common.CyclePermutation(Seq(3, 4))

    val expected = (0 until 10).sortWith(order *: p1 *: p2 *: p3)
    (0 until 10).sortWith(((order *: p1) *: p2) *: p3) should be (expected)
    (0 until 10).sortWith((order *: p1) *: (p2 *: p3)) should be (expected)
    (0 until 10).sortWith((order *: (p1 *: p2)) *: p3) should be (expected)
    (0 until 10).sortWith(order *: ((p1 *: p2) *: p3)) should be (expected)
    (0 until 10).sortWith(order *: (p1 *: (p2 *: p3))) should be (expected)
  }

  "Permutation" should "巡回置換の積に分解可能" in {
    def assertCycleNotation(
        permutation: common.Permutation[Int],
        expectedNotation: List[common.CyclePermutation[Int]]) {
      permutation.cycleNotation.toSet should be (expectedNotation.toSet)
      assert(permutation === permutation.cycleNotation.fold(
        new common.Permutation(Map()))(_ * _))
    }

    assertCycleNotation(
      new common.Permutation(Map(1 -> 2, 2 -> 3, 3 -> 1)),
      List(new common.CyclePermutation(Seq(1, 2, 3))))
    assertCycleNotation(
      new common.Permutation(Map(1 -> 2, 2 -> 3, 3 -> 1, 5 -> 6, 6 -> 5)),
      List(
        new common.CyclePermutation(Seq(1, 2, 3)),
        new common.CyclePermutation(Seq(5, 6))))
  }

  "Permutation" should "文字列の結合と分解が可能" in {
    def assertEncodeAndDecode(texts: Seq[String], expectedEncode: String) {
      assert(common.Permutation.encodeSeq(texts) === expectedEncode)
      common.Permutation.decodeSeq(expectedEncode) should be (texts)
    }

    assertEncodeAndDecode(Seq("hoge"), """hoge""")
    assertEncodeAndDecode(
      Seq("hoge", "fuga", "piyo"), """hoge \ fuga \ piyo""")
    assertEncodeAndDecode(
      Seq("ho\\ge", "fuga", "piyo"), """ho\\ge \ fuga \ piyo""")
    assertEncodeAndDecode(
      Seq("hoge\\", "fuga", "piyo"), """hoge\\ \ fuga \ piyo""")
    assertEncodeAndDecode(
      Seq("ho\\\\ge", "fuga", "piyo"), """ho\\\\ge \ fuga \ piyo""")
    assertEncodeAndDecode(
      Seq("hoge\\\\", "fuga", "piyo"), """hoge\\\\ \ fuga \ piyo""")
  }

  "Permutation" should "置換を文字列に保存・復元可能" in {
    def assertToFromString[T](
        permutation: common.Permutation[T], expected: String,
        targetEncoder: T => String, targetDecoder: String => T) {
      val text = common.Permutation.toString(permutation, targetEncoder)
      assert(text === expected)
      assert(
        permutation === common.Permutation.fromString(text, targetDecoder))
    }

    assertToFromString(
      new common.Permutation(Map()), "",
      (x: String) => x, (x: String) => x)
    assertToFromString(
      new common.Permutation(Map()), "",
      (x: Int) => x.toString, (x: String) => x.toInt)

    assertToFromString(
      new common.Permutation(Map(1 -> 2, 2 -> 3, 3 -> 1)),
      """1 \\ 2 \\ 3""",
      (x: Int) => x.toString, (x: String) => x.toInt)
    assertToFromString(
      new common.Permutation(Map(1 -> 2, 2 -> 3, 3 -> 1, 5 -> 6, 6 -> 5)),
      """1 \\ 2 \\ 3 \ 5 \\ 6""",
      (x: Int) => x.toString, (x: String) => x.toInt)
    assertToFromString(
      new common.Permutation(
        Map(1 -> 2, 2 -> 3, 3 -> 1, 5 -> 6, 6 -> 5, 10 -> 12, 12 -> 10)),
      """1 \\ 2 \\ 3 \ 10 \\ 12 \ 5 \\ 6""",
      (x: Int) => x.toString, (x: String) => x.toInt)

    assertToFromString(
      new common.Permutation(Map("hoge" -> "fuga", "fuga" -> "hoge")),
      """fuga \\ hoge""",
      (x: String) => x, (x: String) => x)
    assertToFromString(
      new common.Permutation(Map(
        "hoge" -> "fuga", "fuga" -> "hoge",
        "foo" -> "bar", "bar" -> "baz", "baz" -> "foo")),
      """bar \\ baz \\ foo \ fuga \\ hoge""",
      (x: String) => x, (x: String) => x)
  }

  "Permutation" should "意図した順序を導く置換を導出可能" in {
    def assertExtract[T](seq: Seq[T], order: (T, T) => Boolean) {
      val permutation = common.Permutation.extract(seq, order)
      seq.sortWith(order *: permutation) should be (seq)
    }

    assertExtract(Seq(), (x: Int, y: Int) => x < y)
    assertExtract(Seq(1, 2, 3), (x: Int, y: Int) => x < y)
    assertExtract(Seq(2, 3, 1), (x: Int, y: Int) => x < y)
    assertExtract(Seq(3, 2, 1), (x: Int, y: Int) => x < y)
    assertExtract(Seq(2, 3, 1, 9, 8), (x: Int, y: Int) => x < y)

    assertExtract(Seq("hoge", "fuga", "piyo"), (x: String, y: String) => x < y)
    assertExtract(Seq("foo", "bar", "baz"), (x: String, y: String) => x < y)
    assertExtract(
      Seq(
        "foo", "bar", "baz", "qux", "quux", "corge", "grault", "garply",
        "waldo", "fred", "plugh", "xyzzy", "thud"),
      (x: String, y: String) => x < y)
    assertExtract(
      Seq("apple", "orange", "banana"), (x: String, y: String) => x < y)
    assertExtract(Seq("ほげ", "ふが", "ぴよ"), (x: String, y: String) => x < y)
    assertExtract(
      Seq("甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"),
      (x: String, y: String) => x < y)
    assertExtract(
      Seq(
        "い", "ろ", "は", "に", "ほ", "へ", "と",
        "ち", "り", "ぬ", "る", "を"),
      (x: String, y: String) => x < y)
    assertExtract(
      Seq(
        "子", "丑", "寅", "卯", "辰", "巳",
        "午", "未", "申", "酉", "戌", "亥"),
      (x: String, y: String) => x < y)
  }
}

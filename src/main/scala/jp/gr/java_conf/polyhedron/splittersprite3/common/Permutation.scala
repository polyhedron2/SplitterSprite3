package jp.gr.java_conf.polyhedron.splittersprite3.common

class InvalidPermutation(message: String) extends Exception(message)

// 置換を表現するクラス
// Permutation[T]はTを入力としてTを出力とする全単射関数である。
// つまり、Tのオブジェクトの入れ替え操作を表現する。
//
// T上の順序定義関数orderについて考える。
// 順序定義関数とはTのオブジェクト２つ(a, b)を受け取り、
// a < bであればtrue, そうでなければfalseを返す関数である。
//
// 順序定義関数orderとPermutation pがある時、新しい順序定義関数newOrderを
// newOrder(a, b) = order(p(a), p(b))により定義できる。
// このnewOrderをorder *: pと表記する。
// つまりnewOrder(a, b) = (order *: p)(a, b)である。
// order *: pはTの順序の一部をpの内容で入れ替えたものとなっている。
//
// 以上からPermutationは順序の入れ替えを表現することもできる。
class Permutation[+T](definition: Map[T, T]) {
  if (definition.keySet != definition.values.toSet) {
    throw new InvalidPermutation(s"${definition}は置換の定義として不正です。")
  }

  // このPermutationで入れ替えを受けるオブジェクト一覧
  def domain[S >: T]: Set[S] =
    definition.filter(kv => kv._1 != kv._2).keySet.asInstanceOf[Set[S]]

  override def equals(that: Any): Boolean = that match {
    case that: Permutation[_] =>
      this.domain == that.domain &&
      this.domain.forall(target => this(target) == that(target))
    case _ => false
  }

  override val toString = s"Permutation(${definition})"

  // targetに対する入れ替え後のオブジェクト
  def apply[S >: T](target: S): S =
    definition.asInstanceOf[Map[S, S]].withDefault(x => x).apply(target)

  // 逆向きの入れ替えを表すPermutation
  lazy val inverse = new Permutation(definition.map(kv => (kv._2, kv._1)))

  // thatの入れ替えをしたあとに、このPermutationの入れ替えをした時の内容を
  // １つで表すPermutation
  // このPermutationを合成関数という。
  // ２つのPermutation p1とp2について、p2の入れ替えをしてからp1の入れ替えをする
  // 合成関数をp1 * p2と表記する。
  def *[S >: T](that: Permutation[S]): Permutation[S] = {
    val newDomain = this.domain ++ that.domain
    val newBody = newDomain.map(key => (key, this(that(key)))).toMap
    new Permutation[S](newBody)
  }

  // p1 * p2はp1 *: p2とも表記できることとする。
  def *:[S >: T](that: Permutation[S]): Permutation[S] = that * this

  // 順序定義関数を元に新しい順序定義関数を返す
  def *:[S >: T](order: (S, S) => Boolean): (S, S) => Boolean =
    (a, b) => order(apply(a), apply(b))

  // 置換は有限個の互いに共通の入れ替え対象を持たない
  // 巡回置換の合成関数として表現できる。
  // 巡回置換とはx_1をx_2に、x_2をx_3に...x_nをx_1にとループするように
  // 入れ替える置換のことである。
  // この関数では、構成要素となる巡回置換の一覧を返す。
  lazy val cycleNotation: List[CyclePermutation[T]] = {
    def extractCycles(targetMap: Map[T, T]): List[CyclePermutation[T]] = {
      def visit(start: T, current: T): List[T] = {
        val next = definition(current)
        if (next == start) current :: Nil else current :: visit(start, next)
      }

      if (targetMap.isEmpty) {
        Nil
      } else {
        val start = targetMap.head._1
        val chain = visit(start, start)
        new CyclePermutation(chain) :: extractCycles(targetMap -- chain)
      }
    }

    extractCycles(definition)
  }
}

// 巡回置換を表すクラス
class CyclePermutation[+T](val chain: Seq[T]) extends Permutation[T](
    if (chain.size != chain.toSet.size) {
      throw new InvalidPermutation(s"${chain}は巡回置換の定義として不正です。")
    } else {
      for (i <- 0 until chain.size) yield {
        if (i == chain.size - 1) {
          (chain(i), chain(0))
        } else {
          (chain(i), chain(i + 1))
        }
      }
    }.toMap) {

  override val toString = s"CyclePermutation(${chain})"
}

object Permutation {
  val escape = "" + '\\'
  val joiner = " \\ "
  // 正規表現のために'\'が追加されている
  val splitter = " \\\\ "

  // 複数の文字列を１つにまとめる
  def encodeSeq(texts: Seq[String]): String =
    texts.map(_.replace(escape, escape * 2)).mkString(joiner)

  // １つにまとめた文字列を分解する
  def decodeSeq(encodedText: String): Seq[String] =
    encodedText.split(splitter).map(_.replace(escape * 2, escape))

  // Permutationを文字列に変換して保存可能にする
  def toString[T](
      permutation: Permutation[T], targetEncoder: T => String): String =
    encodeSeq(
      permutation.cycleNotation.map(_.chain.map(targetEncoder)).map(encodeSeq))

  // 文字列からPermutationを復元する
  def fromString[T](
      encodedText: String, targetDecoder: String => T): Permutation[T] = 
    decodeSeq(encodedText).map(decodeSeq).map(_.map(targetDecoder)).map(
      new CyclePermutation(_).asInstanceOf[Permutation[T]]).reduce(_ * _)

  // seqが(order *: p)による順序通りとなるような、Permutation pを逆算
  def extract[T](seq: Seq[T], order: (T, T) => Boolean) = {
    val definition = seq.sortWith(order).zipWithIndex.map { case (target, i) =>
      (seq(i), target)
    }.toMap
    new Permutation(definition)
  }
}

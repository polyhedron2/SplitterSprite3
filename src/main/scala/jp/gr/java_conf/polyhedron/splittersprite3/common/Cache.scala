package jp.gr.java_conf.polyhedron.splittersprite3.common

class InfiniteLoopCacheException() extends Exception()

// 評価値の記憶用トレイト
trait Cache[KEY, VALUE] {
  // 記憶に用いるマップ
  private var body = Map[KEY, VALUE]()
  // 無限ループ検出用
  private var processing = Set[KEY]()

  def apply(key: KEY): VALUE = synchronized {
    // 多重に評価が実行されているため無限ループとして例外をスロー
    if (processing(key)) { throw new InfiniteLoopCacheException() }

    // 未評価のkeyであれば評価を実行し、結果を記憶
    if (!body.isDefinedAt(key)) {
      try {
        processing += key
        body += (key -> valueFor(key))
      } finally {
        processing -= key
      }
    }

    // 評価結果を返却
    body(key)
  }

  // 評価内容を定義するメソッド
  protected def valueFor(key: KEY): VALUE

  def keys: Iterable[KEY] = body.keys
}

package splittersprite3.spawner

import splittersprite3.spirit.{Spirit}

// XMLファイルを基にインスタンスを生成するトレイト
trait Spawner[+T] {
  // SpawnerはSpirit１つを引数とするコンストラクタを実装しなければ
  // ならないとする
  val spirit: Spirit

  // 処理中に動的に決定される引数の型定義
  type SpawnArgs
  // インスタンス生成を行うメソッド
  protected def _spawn(args: SpawnArgs): T
  def spawn(args: SpawnArgs) = {
    _spawn(args)
  }

  // XMLから読み出すフィールド名・型を取り出すためのフェイク実行用の引数
  def fakeArgs: SpawnArgs
  // XMLから読み出すフィールド名・型を取り出すためのフェイク実行
  lazy val fake = spawn(fakeArgs)
}

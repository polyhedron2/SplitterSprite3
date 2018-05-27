package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import jp.gr.java_conf.polyhedron.splittersprite3.common

// JavaFX Sceneへのキー入力をゲームへの入力に流し込む調整クラス
// 主な役割は
//   1. ゲームがラグった際に同一キーへの連打がされても
//      最低限の量だけゲームプログラムに送り込む
//   2. リプレイ機能のためのキー入力内容の一意性の確保
// この調節のため、キー入力は以下の前提を受けるものとする
//   1. １フレーム内で同一キーの押しおよび放しは１回までに省略される
//     例：'A'「押し」=>'A'「放し」=>'A'「押し」の入力があれば、
//         'A'「押し」のみに省略される
//   2. 同一フレーム内の異なるキー間の押し・放しの順序は保持されず
//      キーの文字列順でソートされる
//     例：'A'「押し」=>'B'「押し」=>'A'「放し」=>'B'「放し」の入力があれば、
//         'A'「押し」=>'A'「放し」=>'B'「押し」=>'B'「放し」になる
// 挙動の詳細はCommandRegulatorSpec参照
class CommandRegulator() {
  // キーが押されているか否かが１フレーム中にどのように切り替わったか
  abstract class InnerKeyCommand {
    // 新たにキーが押された場合の次の内容
    def pressed: InnerKeyCommand
    // 新たにキーが放された場合の次の内容
    def released: InnerKeyCommand
    // 今押されているか否か
    def isPressed: Boolean
    // 実際にゲーム処理に流し込まれるコマンド
    def actualCommandList: List[common.Command]
  }

  // 前フレームから放しっぱなし
  case class AlreadyReleased(text: String) extends InnerKeyCommand {
    def pressed: InnerKeyCommand = Pressed(text)
    def released: InnerKeyCommand = this
    def isPressed: Boolean = false
    def actualCommandList: List[common.Command] = List()
  }
  // このフレームでキーを押した
  case class Pressed(text: String) extends InnerKeyCommand {
    def pressed: InnerKeyCommand = this
    def released: InnerKeyCommand = PressedAndReleased(text)
    def isPressed: Boolean = true
    def actualCommandList: List[common.Command] =
      List(common.KeyPress(text))
  }
  // このフレームでキーを押して放した
  case class PressedAndReleased(text: String) extends InnerKeyCommand {
    // １フレーム内で同一キーを押して放して押すのは１回押した扱い
    def pressed: InnerKeyCommand = Pressed(text)
    def released: InnerKeyCommand = this
    def isPressed: Boolean = false
    def actualCommandList: List[common.Command] =
      List(common.KeyPress(text), common.KeyRelease(text))
  }
  // 前フレームから押しっぱなし
  case class AlreadyPressed(text: String) extends InnerKeyCommand {
    def pressed: InnerKeyCommand = this
    def released: InnerKeyCommand = Released(text)
    def isPressed: Boolean = true
    def actualCommandList: List[common.Command] = List()
  }
  // このフレームで放した
  case class Released(text: String) extends InnerKeyCommand {
    def pressed: InnerKeyCommand = ReleasedAndPressed(text)
    def released: InnerKeyCommand = this
    def isPressed: Boolean = false
    def actualCommandList: List[common.Command] = List(
      common.KeyRelease(text))
  }
  // このフレームで放して押した
  case class ReleasedAndPressed(text: String) extends InnerKeyCommand {
    def pressed: InnerKeyCommand = this
    // １フレーム内で同一キーを放して押して放すのは１回放した扱い
    def released: InnerKeyCommand = Released(text)
    def isPressed: Boolean = true
    def actualCommandList: List[common.Command] =
      List(common.KeyRelease(text), common.KeyPress(text))
  }

  private var commandMap =
    Map[String, InnerKeyCommand]().withDefault(AlreadyReleased)

  // 今フレーム中の「押し」をインプットする
  def enqueuePress(text: String) {
    synchronized { commandMap += text -> commandMap(text).pressed }
  }

  // 今フレーム中の「放し」をインプットする
  def enqueueRelease(text: String) {
    synchronized { commandMap += text -> commandMap(text).released }
  }

  // １フレーム分の入力を返して、次のフレームの準備をする
  def dequeue(): List[common.Command] = {
    synchronized {
      // keyText順にactualCommandListを連結したものが、戻り値
      val ret = commandMap.toList.sortBy(_._1).flatMap(_._2.actualCommandList)
      commandMap =
        // 押されたままのキーのみを残す
        commandMap.toSeq.filter(_._2.isPressed).map(_._1)
          // それらにAlreadyPressedを割り当て
          .map(text => (text, AlreadyPressed(text))).toMap
          // 残りのキーはAlreadyReleased
          .withDefault(AlreadyReleased)
      ret
    }
  }
}

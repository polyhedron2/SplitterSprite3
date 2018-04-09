package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import javafx.scene.input.{KeyEvent}

import jp.gr.java_conf.polyhedron.splittersprite3.common

// Windowへのキー入力をゲームへの入力に流し込む調整クラス
// 主な役割は
//   - １フレーム中に過剰な量のキーボード入力があれば量を調節する
//     特にゲームの処理がラグった際のキー入力量を調節することが目的
//   - マルチメソッド処理時の入力内容の同期
// この調節のため、キー入力は以下の前提を受けるものとする
//   1. １フレーム内で同一キーの押しおよび放しは１回までに省略される
//     例：'A'「押し」=>'A'「放し」=>'A'「押し」の入力があれば、
//         'A'「押し」のみに省略される
//   2. １フレーム内で異なるキーの押しおよび放しは上限なく同時に処理される
//   3. 同一フレーム内の同一キー間の押し・放しの順序は保持される
//   4. 同一フレーム内の異なるキー間の押し・放しの順序は保持されない
//     例：'A'「押し」=>'B'「押し」=>'A'「放し」=>'B'「放し」の入力があれば、
//         'A'と'B'の間の前後関係は考慮されない
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

  def enqueuePress(e: KeyEvent) {
    synchronized {
      commandMap += e.getText() -> commandMap(e.getText()).pressed
    }
  }

  def enqueueRelease(e: KeyEvent) {
    synchronized {
      commandMap += e.getText() -> commandMap(e.getText()).released
    }
  }

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

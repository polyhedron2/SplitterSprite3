package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import java.awt.event.{KeyEvent}

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
//   5. shift, ctrl, altの押し・放しは他のキーよりも先に実行された扱いとなる
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
  case class AlreadyReleased(keyCode: Int) extends InnerKeyCommand {
    def pressed: InnerKeyCommand = Pressed(keyCode)
    def released: InnerKeyCommand = this
    def isPressed: Boolean = false
    def actualCommandList: List[common.Command] = List()
  }
  // このフレームでキーを押した
  case class Pressed(keyCode: Int) extends InnerKeyCommand {
    def pressed: InnerKeyCommand = this
    def released: InnerKeyCommand = PressedAndReleased(keyCode)
    def isPressed: Boolean = true
    def actualCommandList: List[common.Command] = List(
      common.KeyPress(keyCode, shiftIsPressed, ctrlIsPressed, altIsPressed))
  }
  // このフレームでキーを押して放した
  case class PressedAndReleased(keyCode: Int) extends InnerKeyCommand {
    // １フレーム内で同一キーを押して放して押すのは１回押した扱い
    def pressed: InnerKeyCommand = Pressed(keyCode)
    def released: InnerKeyCommand = this
    def isPressed: Boolean = false
    def actualCommandList: List[common.Command] = List(
      common.KeyPress(keyCode, shiftIsPressed, ctrlIsPressed, altIsPressed),
      common.KeyRelease(keyCode))
  }
  // 前フレームから押しっぱなし
  case class AlreadyPressed(keyCode: Int) extends InnerKeyCommand {
    def pressed: InnerKeyCommand = this
    def released: InnerKeyCommand = Released(keyCode)
    def isPressed: Boolean = true
    def actualCommandList: List[common.Command] = List()
  }
  // このフレームで放した
  case class Released(keyCode: Int) extends InnerKeyCommand {
    def pressed: InnerKeyCommand = ReleasedAndPressed(keyCode)
    def released: InnerKeyCommand = this
    def isPressed: Boolean = false
    def actualCommandList: List[common.Command] = List(
      common.KeyRelease(keyCode))
  }
  // このフレームで放して押した
  case class ReleasedAndPressed(keyCode: Int) extends InnerKeyCommand {
    def pressed: InnerKeyCommand = this
    // １フレーム内で同一キーを放して押して放すのは１回放した扱い
    def released: InnerKeyCommand = Released(keyCode)
    def isPressed: Boolean = true
    def actualCommandList: List[common.Command] = List(
      common.KeyRelease(keyCode),
      common.KeyPress(keyCode, shiftIsPressed, ctrlIsPressed, altIsPressed))
  }

  private var keyCodeCommandMap =
    Map[Int, InnerKeyCommand]().withDefault(AlreadyReleased)

  private var shiftIsPressed = false
  private var ctrlIsPressed = false
  private var altIsPressed = false

  def enqueuePress(keyCode: Int) {
    synchronized {
      if (keyCode == KeyEvent.VK_SHIFT) {
        // shiftの押しは先に実行
        shiftIsPressed = true
      } else if (keyCode == KeyEvent.VK_CONTROL) {
        // ctrlの押しは先に実行
        ctrlIsPressed = true
      } else if (keyCode == KeyEvent.VK_ALT) {
        // altの押しは先に実行
        altIsPressed = true
      } else {
        keyCodeCommandMap += keyCode -> keyCodeCommandMap(keyCode).pressed
      }
    }
  }

  def enqueueRelease(keyCode: Int) {
    synchronized {
      if (keyCode == KeyEvent.VK_SHIFT) {
        // shiftの放しは先に実行
        shiftIsPressed = false
      } else if (keyCode == KeyEvent.VK_CONTROL) {
        // ctrlの放しは先に実行
        ctrlIsPressed = false
      } else if (keyCode == KeyEvent.VK_ALT) {
        // altの放しは先に実行
        altIsPressed = false
      } else {
        keyCodeCommandMap += keyCode -> keyCodeCommandMap(keyCode).released
      }
    }
  }

  def dequeue(): List[common.Command] = {
    synchronized {
      // keyCode順にactualCommandListを連結したものが、戻り値
      val ret =
        keyCodeCommandMap.toList.sortBy(_._1).flatMap(_._2.actualCommandList)
      keyCodeCommandMap =
        // 押されたままのキーのみを残す
        keyCodeCommandMap.toSeq.filter(_._2.isPressed).map(_._1)
          // それらにAlreadyPressedを割り当て
          .map(keyCode => (keyCode, AlreadyPressed(keyCode))).toMap
          // 残りのキーはAlreadyReleased
          .withDefault(AlreadyReleased)
      ret
    }
  }
}

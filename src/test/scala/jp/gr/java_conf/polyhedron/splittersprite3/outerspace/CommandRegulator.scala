import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}

import jp.gr.java_conf.polyhedron.splittersprite3.common
import jp.gr.java_conf.polyhedron.splittersprite3.outerspace


class CommandRegulatorSpec
    extends FlatSpec with DiagrammedAssertions with Matchers {
  "CommandRegulator" should "何もしなかったフレームでは何も返らない" in {
    val regulator = new outerspace.CommandRegulator()
    // 直前のフレームで押しっぱなしのキー
    regulator.enqueuePress("Pressed")
    // 直前のフレームで放しっぱなしのキー
    regulator.enqueuePress("Released")
    regulator.enqueueRelease("Released")
    // 次のフレームに進める
    regulator.dequeue()

    // このフレームでは何も操作がされずに次のフレームに進む
    val results = regulator.dequeue()
    results should be (List())
  }

  "CommandRegulator" should "同じ状態を連続で入力しても無視される" in {
    val regulator = new outerspace.CommandRegulator()
    // 直前のフレームで押しっぱなしのキー
    regulator.enqueuePress("Pressed")
    // 直前のフレームで放しっぱなしのキー
    regulator.enqueuePress("Released")
    regulator.enqueueRelease("Released")
    // 次のフレームに進める
    regulator.dequeue()

    // このフレームでは前フレームの最終状態と同じ入力がされる
    regulator.enqueuePress("Pressed")
    regulator.enqueueRelease("Released")
    val results = regulator.dequeue()
    results should be (List())
  }

  "CommandRegulator" should
      "フレーム中に「放し」から「押し」になればKeyPress一回のみの扱い" in {
    // すべてのキーは「放し」状態
    val regulator = new outerspace.CommandRegulator()

    // 一回押すだけのキー
    regulator.enqueuePress("once")

    // 「押し」→「放し」→「押し」
    regulator.enqueuePress("press-release-press")
    regulator.enqueueRelease("press-release-press")
    regulator.enqueuePress("press-release-press")

    // たくさんキーを押すが、最後は押した状態
    for (i <- 0 until 100) {
      regulator.enqueuePress("many")
      regulator.enqueueRelease("many")
    }
    regulator.enqueuePress("many")

    val results = regulator.dequeue()
    // 各キーでKeyPress一回ずつ, 順序はキーのソート順
    results should be (List(
      common.KeyPress("many"),
      common.KeyPress("once"),
      common.KeyPress("press-release-press")))
  }

  "CommandRegulator" should
      "フレーム中に「押し」から「放し」になればKeyRelease一回のみの扱い" in {
    val regulator = new outerspace.CommandRegulator()
    regulator.enqueuePress("once")
    regulator.enqueuePress("release-press-release")
    regulator.enqueuePress("many")
    // 直前のフレームで３つのキーは押しっぱなし
    regulator.dequeue()

    // 一回放すだけのキー
    regulator.enqueueRelease("once")

    // 「放し」→「押し」→「放し」
    regulator.enqueueRelease("release-press-release")
    regulator.enqueuePress("release-press-release")
    regulator.enqueueRelease("release-press-release")

    // たくさんキーを押すが、最後は放した状態
    for (i <- 0 until 100) {
      regulator.enqueueRelease("many")
      regulator.enqueuePress("many")
    }
    regulator.enqueueRelease("many")

    val results = regulator.dequeue()
    // 各キーでKeyRelease一回ずつ, 順序はキーのソート順
    results should be (List(
      common.KeyRelease("many"),
      common.KeyRelease("once"),
      common.KeyRelease("release-press-release")))
  }

  "CommandRegulator" should
      "フレーム中に「放し」から「押し」→「放し」でKeyPress & KeyRelease" in {
    // すべてのキーは「放し」状態
    val regulator = new outerspace.CommandRegulator()

    // 一回押すだけのキー
    regulator.enqueuePress("once")
    regulator.enqueueRelease("once")

    // 「押し」→「放し」
    regulator.enqueuePress("twice")
    regulator.enqueueRelease("twice")
    regulator.enqueuePress("twice")
    regulator.enqueueRelease("twice")

    // たくさんキーを押すが、最後は放した状態
    for (i <- 0 until 100) {
      regulator.enqueuePress("many")
      regulator.enqueueRelease("many")
    }

    val results = regulator.dequeue()
    // 各キーでKeyPress一回ずつ, 順序はキーのソート順
    results should be (List(
      common.KeyPress("many"), common.KeyRelease("many"),
      common.KeyPress("once"), common.KeyRelease("once"),
      common.KeyPress("twice"), common.KeyRelease("twice")))
  }

  "CommandRegulator" should
      "フレーム中に「押し」から「放し」→「押し」でKeyRelease & KeyPress" in {
    val regulator = new outerspace.CommandRegulator()
    regulator.enqueuePress("once")
    regulator.enqueuePress("twice")
    regulator.enqueuePress("many")
    // 直前のフレームで３つのキーは押しっぱなし
    regulator.dequeue()

    // 一回放すだけのキー
    regulator.enqueueRelease("once")
    regulator.enqueuePress("once")

    // 「放し」→「押し」
    regulator.enqueueRelease("twice")
    regulator.enqueuePress("twice")
    regulator.enqueueRelease("twice")
    regulator.enqueuePress("twice")

    // たくさんキーを押すが、最後は放した状態
    for (i <- 0 until 100) {
      regulator.enqueueRelease("many")
      regulator.enqueuePress("many")
    }

    val results = regulator.dequeue()
    // 各キーでKeyPress一回ずつ, 順序はキーのソート順
    results should be (List(
      common.KeyRelease("many"), common.KeyPress("many"),
      common.KeyRelease("once"), common.KeyPress("once"),
      common.KeyRelease("twice"), common.KeyPress("twice")))
  }
}

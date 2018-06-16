package jp.gr.java_conf.polyhedron.splittersprite3.vanilla

import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{Spirit}
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{OutermostSpawner}


class CycleSpawner(val spirit: Spirit)
    extends OutermostSpawner[Cycle] {
  val initialModeName = spirit.string("開始モード名")
  lazy val modeMap =
    spirit.withString.andInnerSpawner[ModeSpawner].map("モード設定").map {
      case (modeName, modeSpawner) => (modeName, modeSpawner.spawn(()))
    }

  type SpawnArgs = Unit
  override val fakeArgs = ()
  def createInstance(x: Unit) = new Cycle(initialModeName, modeMap)
}

class GameOverException() extends Exception()

class Cycle(initialModeName: String, modeMap: => Map[String, Mode]) {
  val initialMode = modeMap(initialModeName)
  var modeStack = collection.immutable.Stack(initialMode)
  initialMode.enter()

  def update() {
    val mode = try {
      modeStack.top
    } catch {
      // stackが空の場合はゲーム終了
      case e: NoSuchElementException => throw new GameOverException()
    }

    mode.effect()

    val operation = mode.clock()
    operation match {
      case Stay =>
      case Pop => {
        mode.exit()
        modeStack = modeStack.pop
      }
      case Push(nextModeName) => {
        val nextMode = modeMap(nextModeName)
        modeStack = modeStack.push(nextMode)
        nextMode.enter()
      }
      case Replace(nextModeName) => {
        mode.exit()
        modeStack = modeStack.pop

        val nextMode = modeMap(nextModeName)
        modeStack = modeStack.push(nextMode)
        nextMode.enter()
      }
    }
  }
}

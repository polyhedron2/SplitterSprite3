package jp.gr.java_conf.polyhedron.splittersprite3.vanilla

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.common
import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{Spirit}
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{OutermostSpawner}

abstract class ControllerSpawner(val spirit: Spirit)
    extends OutermostSpawner[Controller] {
  type SpawnArgs = Unit
  override val fakeArgs = ()
}

class GameOverException() extends Exception()

abstract class Controller() {
  def initialState: State
  var stateStack = collection.immutable.Stack(initialState)
  initialState.model.enter()

  def update() {
    val state = try {
      stateStack.top
    } catch {
      // stackが空の場合はゲーム終了
      case e: NoSuchElementException => throw new GameOverException()
    }

    state.render()

    val operation = state.update()
    operation match {
      case Stay =>
      case Pop => {
        state.model.exit()
        stateStack = stateStack.pop
      }
      case Push(nextState) => {
        stateStack = stateStack.push(nextState)
        nextState.model.enter()
      }
      case Replace(nextState) => {
        state.model.exit()
        stateStack = stateStack.pop
        stateStack = stateStack.push(nextState)
        nextState.model.enter()
      }
    }
  }

  abstract class State {
    type ModelState

    def modelState2operation(modelState: ModelState): StackOperation
    def model: Model[ModelState]
    // windowNameごとの描画View
    def viewSeqMap: Map[String, Seq[View]]

    def update(): StackOperation = {
      for (windowName <- Atmosphere.commandRegulator.keys) {
        for (command <- Atmosphere.commandRegulator(windowName).dequeue()) {
          model.command(windowName, command)
        }
      }
      val modelState = model.update()
      modelState2operation(modelState)
    }

    def render() {
      for ((windowName, viewSeq) <- viewSeqMap) {
        viewSeq.foreach(_.render(Atmosphere.graphicsContext(windowName)))
      }
    }
  }

  sealed abstract class StackOperation
  case object Stay extends StackOperation
  case object Pop extends StackOperation
  case class Push(state: State) extends StackOperation
  case class Replace(state: State) extends StackOperation
}

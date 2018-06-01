package jp.gr.java_conf.polyhedron.splittersprite3

import java.lang.{Boolean => JBoolean}

import javafx.animation.{AnimationTimer}
import javafx.application.{Application, Platform}
import javafx.beans.value.{ObservableValue, ChangeListener}
import javafx.event.{EventHandler}
import javafx.geometry.{Pos}
import javafx.scene.image.{ImageView}
import javafx.scene.{Scene}
import javafx.scene.input.{KeyEvent}
import javafx.scene.layout.{StackPane}
import javafx.stage.{Stage}

// ゲームの開始地点となるシングルトン
object Main {
  def main(args: Array[String]) {
    Application.launch(classOf[Main])
  }
}

class Main() extends Application {
  val gameName = "name"
  val mode = "main"
  val width = 900
  val height = 900
  var primaryStageOpt: Option[Stage] = None

  def start(primaryStage: Stage) {
    new Thread() {
      override def run() {
        agent.LoanAgent.loan {
          Atmosphere.javaFXTaskQueue.enqueue(setUp)

          val spawner = spirit.OutermostRealSpirit(
            "main.spirit").spawner.asInstanceOf[vanilla.ControllerSpawner]
          val controller = spawner.spawn(())
          val haltGame = agent.ThreadPool.startAndGetHalter(
            new agent.ThreadPool.IntervalRunnable {
              override val fps = 60
              override def intervalRunOnce() = {
                controller.update()
                true
              }
            })

          primaryStage.showingProperty().addListener(
            new ChangeListener[JBoolean] {
              def changed(o: ObservableValue[_ <: JBoolean],
                          before: JBoolean, after: JBoolean) {
                if (before && !after) { haltGame() }
              }
            })
        }
        Platform.exit()
      }
    }.start()

    new AnimationTimer() {
      override def handle(now: Long) {
        try {
          val operation = Atmosphere.javaFXTaskQueue.dequeue()
          operation(primaryStage)
          handle(now)
        } catch {
          case e: NoSuchElementException =>
        }
      }
    }.start()
  }

  def setUp(primaryStage: Stage) {
    primaryStageOpt = Some(primaryStage)
    primaryStage.setTitle(gameTitle)

    val pane = new StackPane()
    pane.setAlignment(Pos.CENTER)

    val scene = new Scene(pane, width, height)
    primaryStage.setScene(scene)

    val logoView = new ImageView(Resources.logo)
    pane.getChildren().add(logoView)

    primaryStage.show()

    scene.setOnKeyPressed(
      new EventHandler[KeyEvent]() {
        def handle(e: KeyEvent) {
          Atmosphere.commandRegulator("main").enqueuePress(e.getText())
        }
      })

    scene.setOnKeyReleased(
      new EventHandler[KeyEvent]() {
        def handle(e: KeyEvent) {
          Atmosphere.commandRegulator("main").enqueueRelease(e.getText())
        }
      })
  }

  // FPS計算値(未計算ならNone)
  private var fpsOpt: Option[Int] = None

  private def gameTitle = synchronized {
    s"${gameName} ${Atmosphere.ioUtils.versionName}" +
    (if (mode == "main") { "" } else { s" MODE=${mode}" }) +
    fpsOpt.map(fps => f"[FPS:${fps}%.2f]").getOrElse("")
  }

  // FPS値の更新
  def fps_=(fps: Int) {
    fpsOpt = Some(fps)
    primaryStageOpt.foreach(_.setTitle(gameTitle))
  }
}

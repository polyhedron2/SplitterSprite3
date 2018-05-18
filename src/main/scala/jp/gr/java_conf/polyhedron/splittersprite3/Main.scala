package jp.gr.java_conf.polyhedron.splittersprite3

import javafx.application.{Application, Platform}
import javafx.event.{EventHandler}
import javafx.stage.{Stage}
import javafx.scene.{Group, Scene}
import javafx.scene.canvas.{Canvas}
import javafx.scene.input.{KeyEvent}

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
    agent.LoanAgent.loan {
      enter(primaryStage)
      val spawner = spirit.OutermostRealSpirit(
        "main.xml").spawner.asInstanceOf[vanilla.ControllerSpawner]
      val controller = spawner.spawn(())
      agent.ThreadPool.startAndGetHalter(
        new agent.ThreadPool.IntervalRunnable {
          override def fps = 60
          override def intervalRunOnce() = {
            controller.update()
            true
          }
        })
    }
    Platform.exit()
  }

  def enter(primaryStage: Stage) {
    primaryStageOpt = Some(primaryStage)
    primaryStage.setTitle(gameTitle)

    val root = new Group()
    val scene = new Scene(root)
    val canvas = new Canvas(width, height)

    primaryStage.setScene(scene)
    root.getChildren().add(canvas)

    scene.setOnKeyPressed(
      new EventHandler[KeyEvent]() {
        def handle(e: KeyEvent) {
          Atmosphere.commandRegulator("main").enqueuePress(e)
        }
      })

    scene.setOnKeyReleased(
      new EventHandler[KeyEvent]() {
        def handle(e: KeyEvent) {
          Atmosphere.commandRegulator("main").enqueueRelease(e)
        }
      })

    Atmosphere.graphicsContext("main") = canvas.getGraphicsContext2D()
    primaryStage.show()
  }

  // FPS計算値(未計算ならNone)
  private var fpsOpt: Option[Int] = None

  private def gameTitle = synchronized {
    s"${gameName} ${Atmosphere.ioUtils.versionName}" +
    (if (mode == "main") { "" } else { s" MODE=${mode}" }) +
    fpsOpt.map(fps => f"[FPS:${fps}%.2f]")
  }

  // FPS値の更新
  def fps_=(fps: Int) {
    fpsOpt = Some(fps)
    primaryStageOpt.foreach(_.setTitle(gameTitle))
  }
}

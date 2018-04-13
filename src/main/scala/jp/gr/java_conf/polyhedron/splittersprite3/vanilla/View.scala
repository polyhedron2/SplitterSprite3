package jp.gr.java_conf.polyhedron.splittersprite3.vanilla

import javafx.scene.canvas.{GraphicsContext}

abstract class View() {
  def render(graphicsContext: GraphicsContext)
}

package jp.gr.java_conf.polyhedron.splittersprite3

import javafx.scene.image.{Image}

// Jarファイル内のリソースアクセス用シングルトン
object Resources {
  val logo = new Image(
    getClass().getClassLoader().getResourceAsStream("logo.png"))
}

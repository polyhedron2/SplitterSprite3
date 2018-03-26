package jp.gr.java_conf.polyhedron.splittersprite3.common

sealed abstract class Command
case class KeyPress(
  keyCode: Int,
  shiftIsPressed: Boolean, ctrlIsPressed: Boolean, altIsPressed: Boolean)
  extends Command
case class KeyRelease(keyCode: Int) extends Command

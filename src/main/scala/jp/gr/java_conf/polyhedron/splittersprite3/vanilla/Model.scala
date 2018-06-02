package jp.gr.java_conf.polyhedron.splittersprite3.vanilla

import jp.gr.java_conf.polyhedron.splittersprite3.common

abstract class Model[MODEL_STATE] {
  def command(windowName: String, command: common.Command)
  def update(): MODEL_STATE
  def enter() { }
  def exit() { }
}

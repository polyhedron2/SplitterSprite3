package jp.gr.java_conf.polyhedron.splittersprite3.vanilla

import jp.gr.java_conf.polyhedron.splittersprite3.common
import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{Spirit}
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{OutermostSpawner}


abstract class Clock() {
  def returnEnum: Enumeration
  def command(windowName: String, command: common.Command)
  def forward(): returnEnum.Value
  def enter() { }
  def exit() { }
}

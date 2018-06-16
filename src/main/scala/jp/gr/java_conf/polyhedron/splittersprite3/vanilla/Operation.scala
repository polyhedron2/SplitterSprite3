package jp.gr.java_conf.polyhedron.splittersprite3.vanilla

import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{Spirit}
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{InnerSpawner}

abstract class OperationSpawner(val spirit: Spirit)
    extends InnerSpawner[Operation] {
  type SpawnArgs = Unit
  override val fakeArgs = ()
}

class StaySpawner(spirit: Spirit) extends OperationSpawner(spirit) {
  def createInstance(x: Unit) = Stay
}

class PopSpawner(spirit: Spirit) extends OperationSpawner(spirit) {
  def createInstance(x: Unit) = Pop
}

class PushSpawner(spirit: Spirit) extends OperationSpawner(spirit) {
  def createInstance(x: Unit) = Push(spirit.string("上乗せモード名"))
}

class ReplaceSpawner(spirit: Spirit) extends OperationSpawner(spirit) {
  def createInstance(x: Unit) = Replace(spirit.string("遷移先モード名"))
}

sealed abstract class Operation
case object Stay extends Operation
case object Pop extends Operation
case class Push(modeName: String) extends Operation
case class Replace(modeName: String) extends Operation

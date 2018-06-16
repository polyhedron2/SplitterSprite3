package jp.gr.java_conf.polyhedron.splittersprite3.vanilla

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{Spirit}
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{InnerSpawner}


abstract class ModeSpawner(val spirit: Spirit) extends InnerSpawner[Mode] {
  val returnEnum: Enumeration
  def clock: Clock

  assert(clock.returnEnum == returnEnum)

  lazy val effectSeq = spirit.withOutermostSpawner[
    EffectSpawner].seq("エフェクト群").map(_.spawn(()))
  lazy val transition = returnEnum.values.map { case enumVal =>
    val operation = spirit("モード遷移内容").innerSpawner[
      OperationSpawner](enumVal.toString).spawn(())
    (enumVal.toString, operation)
  }.toMap

  type SpawnArgs = Unit
  override val fakeArgs = ()
  def createInstance(x: Unit) = new Mode(clock, effectSeq, transition)
}

class Mode(clock: => Clock, effectSeq: => Seq[Effect],
           transition: => Map[String, Operation]) {
  def clock(): Operation = {
    for (windowName <- Atmosphere.commandRegulator.keys) {
      for (command <- Atmosphere.commandRegulator(windowName).dequeue()) {
        clock.command(windowName, command)
      }
    }
    transition(clock.forward().toString)
  }
  def effect() { effectSeq.foreach(_.apply()) }
  def enter() { clock.enter() }
  def exit() { clock.exit() }
}

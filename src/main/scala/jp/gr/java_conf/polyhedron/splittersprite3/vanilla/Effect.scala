package jp.gr.java_conf.polyhedron.splittersprite3.vanilla

import jp.gr.java_conf.polyhedron.splittersprite3.spirit.{Spirit}
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{OutermostSpawner}


abstract class EffectSpawner(val spirit: Spirit)
    extends OutermostSpawner[Effect] {
  type SpawnArgs = Unit
  override val fakeArgs = ()
}

abstract class Effect() {
  def apply()
}

package jp.gr.java_conf.polyhedron.splittersprite3.common

class SynchronizedMap[KEY, VALUE]() {
  private var body = Map[KEY, VALUE]()
  def apply(key: KEY) = synchronized { body(key) }
  def update(key: KEY, value: VALUE) {
    synchronized { body += key -> value }
  }
}

package jp.gr.java_conf.polyhedron.splittersprite3.agent

trait Agent { def open() = {}; def close() = {} }

object Agent {
  lazy val agents = Seq(Logger, Specificator)

  def open() { agents.foreach(_.open()) }

  def close() { agents.reverse.foreach(a => try {
    a.close()
  } catch {
    case e: Exception => if (Logger.is_opened) {
      Logger.printStackTrace(e, Error)
    } else {
      e.printStackTrace()
    }
  }) }

  def withAgents(operation: Unit => Any) {
    try {
      open()
      operation()
    } catch {
      case e: Exception => Logger.printStackTrace(e)
    } finally {
      close()
    }
  }
}

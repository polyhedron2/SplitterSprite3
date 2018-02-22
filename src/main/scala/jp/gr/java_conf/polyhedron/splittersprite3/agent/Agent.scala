package jp.gr.java_conf.polyhedron.splittersprite3.agent

class AgentDoublyUsed(a: Agent)
  extends Exception(s"エージェント${a}が多重に起動されました。")

trait Agent {
  protected def enter() = {}
  protected def exit(exOpt: Option[Exception]) = exOpt.foreach { throw _ }
  private var alreadyUsing = false
  def loan(operation: => Any) = synchronized {
    if (alreadyUsing) { throw new AgentDoublyUsed(this) }

    val exOpt = try {
      alreadyUsing = true
      enter()
      operation
      None
    } catch {
      case e: Exception => Some(e)
    } finally {
      alreadyUsing = false
    }

    exit(exOpt)
  }
}

object Agent {
  def loan(operation: => Any) = try {
    Logger.loan { ThreadPool.loan { Specificator.loan { operation } } }
  } catch {
    case e: Exception => e.printStackTrace()
  }
}

package jp.gr.java_conf.polyhedron.splittersprite3.agent

class AgentDoublyUsed(a: Agent)
  extends Exception(s"エージェント${a}が多重に起動されました。")

//開始処理と終了処理を定めるシングルトン用のトレイト
trait Agent {
  //開始処理
  protected def enter() = {}
  //終了処理
  //exOpt: 途中で例外が発生したらSome(ex), なければNone
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
  lazy val agents = List(Logger, ThreadPool, Specificator)

  private def _loan(operation: => Any, agentList: List[Agent]) {
    agentList match {
      case head :: tail => head.loan { _loan(operation, tail) }
      case Nil => operation
    }
  }

  def loan(operation: => Any) = try {
    _loan(operation, agents)
  } catch {
    case e: Exception => e.printStackTrace()
  }
}

package jp.gr.java_conf.polyhedron.splittersprite3.agent

class AgentDoublyUsed(a: LoanAgent)
  extends Exception(s"エージェント${a}が多重に起動されました。")

//開始処理と終了処理を定めるシングルトン用のトレイト
trait LoanAgent {
  //開始処理
  protected def enter() = {}
  //終了処理
  //exOpt: 途中で例外が発生したらSome(ex), なければNone
  protected def exit(exOpt: Option[Exception]) = exOpt.foreach { throw _ }

  private var alreadyUsing = false
  def loan(operation: => Any) {
    if (alreadyUsing) { throw new AgentDoublyUsed(this) }
    synchronized {
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
}

object LoanAgent {
  lazy val agents = List(Logger, ThreadPool)

  private def loan(operation: => Any, agentList: List[LoanAgent]) {
    agentList match {
      case head :: tail => head.loan { loan(operation, tail) }
      case Nil => operation
    }
  }

  def loan(operation: => Any) {
    try {
      loan(operation, agents)
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}

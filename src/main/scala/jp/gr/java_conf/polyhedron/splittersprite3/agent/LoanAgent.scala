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

  protected var alreadyUsing = false
  def loan(operation: => Any): Boolean = {
    synchronized {
      if (alreadyUsing) { throw new AgentDoublyUsed(this) }
      alreadyUsing = true
    }

    val success = try {
      enter()
      operation
      exit(None)
      true
    } catch {
      case e: Exception => {
        exit(Some(e))
        false
      }
    }

    synchronized { alreadyUsing = false }
    success
  }
}

object LoanAgent {
  lazy val agents = List(Logger, ThreadPool)

  // 戻り値は成功終了か否か
  private def loan(operation: => Any, agentList: List[LoanAgent]): Boolean = {
    agentList match {
      case head :: tail => head.loan { loan(operation, tail) }
      case Nil => {
        operation
        true
      }
    }
  }

  // 戻り値は成功終了か否か
  def loan(operation: => Any): Boolean = {
    try {
      loan(operation, agents)
    } catch {
      case e: Exception => {
        e.printStackTrace()
        false
      }
    }
  }
}

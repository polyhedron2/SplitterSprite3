package jp.gr.java_conf.polyhedron.splittersprite3.agent

import org.scalatest.exceptions.{TestFailedException}

class AgentDoublyUsed(a: LoanAgent)
  extends Exception(s"エージェント${a}が多重に起動されました。")

trait LoanAgentGoingThroughException

//開始処理と終了処理を定めるシングルトン用のトレイト
trait LoanAgent {
  //開始処理
  protected def enter() = {}
  //終了処理
  //exOpt: 途中で例外が発生したらSome(ex), なければNone
  protected def exit(exOpt: Option[Exception]) = exOpt.foreach { throw _ }

  protected var alreadyUsing = false

  def loan(operation: => Any) {
    synchronized {
      if (alreadyUsing) { throw new AgentDoublyUsed(this) }
      alreadyUsing = true
    }

    try {
      val exOpt = try {
        enter()
        operation
        None
      } catch {
        // テストのアサーションエラーはそのままthrow
        case e: TestFailedException => throw e
        case e: LoanAgentGoingThroughException => throw e
        case e: Exception => Some(e)
      }
      exit(exOpt)
    } finally {
      synchronized { alreadyUsing = false }
    }
  }
}

object LoanAgent {
  lazy val agents = List(Logger, ThreadPool)

  def loan(operation: => Any, agentList: List[LoanAgent]) {
    synchronized {
      agentList match {
        case head :: tail => head.loan { loan(operation, tail) }
        case Nil => {
          Logger.infoLog("All agents are successfully started.")
          operation
          Logger.infoLog("Game starting process is successfully finished.")
          true
        }
      }
    }
  }

  def loan(operation: => Any) { loan(operation, agents) }
}

package jp.gr.java_conf.polyhedron.splittersprite3.agent

import org.scalatest.exceptions.TestFailedException

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

  // 戻り値は成功終了か否か
  def loan(operation: => Any): Boolean = {
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
        case e: Exception => Some(e)
      }
      exit(exOpt)

      // exOptがNoneであれば成功終了
      exOpt.isEmpty
    } finally {
      synchronized { alreadyUsing = false }
    }
  }
}

object LoanAgent {
  lazy val agents = List(Logger, ThreadPool)

  // 戻り値は成功終了か否か
  private def loan(operation: => Any, agentList: List[LoanAgent]): Boolean = {
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

  // 戻り値は成功終了か否か
  def loan(operation: => Any): Boolean = {
    try {
      loan(operation, agents)
    } catch {
      // テストのアサーションエラーはそのままthrow
      case e: TestFailedException => throw e
      case e: Exception => {
        e.printStackTrace()
        false
      }
    }
  }
}

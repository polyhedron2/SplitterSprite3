import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}
import org.scalatest.exceptions.{TestFailedException}

import jp.gr.java_conf.polyhedron.splittersprite3.Atmosphere
import jp.gr.java_conf.polyhedron.splittersprite3.agent

class LoanAgent extends FlatSpec with DiagrammedAssertions with Matchers {
  "LoanAgent" should "デフォルトのexitで例外をそのままthrow" in {
    class TestedException() extends Exception()

    val loanAgent = new agent.LoanAgent() {}
    intercept[TestedException] {
      loanAgent.loan { throw new TestedException() }
    }
  }

  "LoanAgent" should "処理中に例外が発生すればexitの引数となる" in {
    case class TestedException() extends Exception()

    val loanAgent = new agent.LoanAgent() {
      override def exit(exOpt: Option[Exception]) {
        exOpt should be (Some(TestedException()))
      }
    }
    loanAgent.loan { throw TestedException() }
  }

  "LoanAgent" should "二重起動されれば例外" in {
    val loanAgent = new agent.LoanAgent() {}
    intercept[agent.AgentDoublyUsed] {
      loanAgent.loan { loanAgent.loan {} }
    }
  }

  "LoanAgent" should "LoanAgentインスタンスはテストの例外をそのままthrow" in {
    val loanAgent = new agent.LoanAgent() {}
    val testException = intercept[TestFailedException] { assert(false) }
    val e = intercept[TestFailedException] {
      loanAgent.loan { throw testException }
    }
    assert(e === testException)
  }

  "LoanAgent" should "LoanAgentコンパニオンはテストの例外をそのままthrow" in {
    val testException = intercept[TestFailedException] { assert(false) }
    Atmosphere.withTestIOUtils {
      val e = intercept[TestFailedException] {
        agent.LoanAgent.loan { throw testException }
      }
      assert(e === testException)
    }
  }

  "LoanAgent" should "loan実行順にenter、逆順にexit" in {
    var checkList = List[String]()

    val firstAgent = new agent.LoanAgent() {
      override def enter() { checkList :+= "first enter" }
      override def exit(exOpt: Option[Exception]) {
        checkList :+= "first exit" }
    }

    val secondAgent = new agent.LoanAgent() {
      override def enter() { checkList :+= "second enter" }
      override def exit(exOpt: Option[Exception]) {
        checkList :+= "second exit" }
    }

    val thirdAgent = new agent.LoanAgent() {
      override def enter() { checkList :+= "third enter" }
      override def exit(exOpt: Option[Exception]) {
        checkList :+= "third exit" }
    }

    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan({
        checkList :+= "operation"
      }, List(firstAgent, secondAgent, thirdAgent))
    }

    checkList should be (List(
      "first enter", "second enter", "third enter", "operation",
      "third exit", "second exit", "first exit"))
  }
}

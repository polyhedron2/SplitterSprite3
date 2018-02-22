package jp.gr.java_conf.polyhedron.splittersprite3

// ゲームの開始地点となるシングルトン
object Spore {
  def main(args: Array[String]) {
    // 仮実装
    agent.Agent.withAgents { _ =>
      agent.Logger.infoLog("Hello World!!")
    }
  }
}

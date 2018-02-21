package jp.gr.java_conf.polyhedron.splittersprite3

// ゲームの開始地点となるシングルトン
object Spore {
  def main(args: Array[String]) {
    // 仮実装
    try {
      agent.Logger.infoLog(
        "================== SYSTEM PROPERTY ==================")
      showPropertyInfo("java.version")
      showPropertyInfo("java.runtime.version")
      showPropertyInfo("java.runtime.name")
      showPropertyInfo("java.vm.version")
      showPropertyInfo("java.vm.name")
      agent.Logger.infoLog(
        "=====================================================")
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      agent.Logger.close()
    }
  }

  def showPropertyInfo(key: String) {
    agent.Logger.infoLog(s"${key}: ${System.getProperty(key)}")
  }
}

package jp.gr.java_conf.polyhedron.splittersprite3

// ゲームの開始地点となるシングルトン
object Spore {
  def main(args: Array[String]) {
    // 仮実装
    try {
      common.Logger.infoLog(
        "================== SYSTEM PROPERTY ==================")
      showPropertyInfo("java.version")
      showPropertyInfo("java.runtime.version")
      showPropertyInfo("java.runtime.name")
      showPropertyInfo("java.vm.version")
      showPropertyInfo("java.vm.name")
      common.Logger.infoLog(
        "=====================================================")
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      common.Logger.close()
    }
  }

  def showPropertyInfo(key: String) {
    common.Logger.infoLog(s"${key}: ${System.getProperty(key)}")
  }
}

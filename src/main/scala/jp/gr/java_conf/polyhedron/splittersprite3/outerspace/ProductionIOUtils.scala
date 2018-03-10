package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import java.io.{PrintStream}
import java.nio.file.{Paths}

class ProductionIOUtils() extends IOUtils {
  override val stderr = new PrintStream(System.err, true, "UTF-8")
  override val gameJarPath =
    Paths.get(System.getProperty("java.class.path")).toAbsolutePath()
}

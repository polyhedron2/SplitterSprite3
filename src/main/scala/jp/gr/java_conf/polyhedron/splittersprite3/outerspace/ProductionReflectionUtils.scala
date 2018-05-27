package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import java.util.jar.{JarEntry, JarFile}
import scala.collection.JavaConverters._

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}
import jp.gr.java_conf.polyhedron.splittersprite3.agent

class ProductionReflectionUtils() extends ReflectionUtils {
  lazy val jarFile = new JarFile(Atmosphere.ioUtils.gameJarPath.toString)

  override def classIterator: Iterator[Class[_]] = {
    val stream = jarFile.stream()
    try {
      val classFileNames =
        stream.iterator().asScala.map(_.getName()).filter(_.endsWith(".class"))
      val classNames =
        classFileNames.map(_.dropRight(".class".length).replace('/', '.'))
      classNames.flatMap { case className =>
        try {
          Some(Class.forName(className).asInstanceOf[Class[Any]])
        } catch {
          case e: Error => {
            agent.Logger.infoLog(
              s"${className} is skipped in classIterator. Because of ${e}.")
            None
          }
          case e: Exception => {
            agent.Logger.infoLog(
              s"${className} is skipped in classIterator. Because of ${e}.")
            None
          }
        }
      }
    } finally {
      stream.close()
    }
  }
}

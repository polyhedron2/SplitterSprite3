package jp.gr.java_conf.polyhedron.splittersprite3.common

import java.util.jar.{JarEntry, JarFile}
import scala.collection.JavaConverters._
import scala.reflect.{ClassTag}

import jp.gr.java_conf.polyhedron.splittersprite3.agent

// リフレクション関連の処理を扱うシングルトン
object Reflector {
  def typeOf[T: ClassTag] =
    implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]

  lazy val jarFile = new JarFile(System.getProperty("java.class.path"))
  def classIterator = {
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

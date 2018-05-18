package jp.gr.java_conf.polyhedron.splittersprite3.common

import java.lang.reflect.{Modifier}
import java.util.jar.{JarEntry, JarFile}
import scala.collection.JavaConverters._
import scala.reflect.{ClassTag}

import jp.gr.java_conf.polyhedron.splittersprite3.agent
import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{Spawner}

// リフレクション関連の処理を扱うシングルトン
object Reflector {
  def typeOf[T: ClassTag] =
    implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]

  lazy val jarFile = new JarFile(System.getProperty("java.class.path"))
  def classIterator: Iterator[Class[_]] = {
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

	// 無名クラスか否か
  // $Nで終わるクラス名は無名クラスに対応する
  def isAnonymous(cls: Class[_]) = try {
    val clsName = cls.toString().split("\\$", 0)
    val tailName = clsName(clsName.size - 1)
    tailName.toInt
    true
  } catch {
    case e: NumberFormatException => false
  }

  lazy val spawnerClassList: List[Class[_ <: Spawner[Any]]] = classIterator.flatMap {
    case cls: Class[_] => if (classOf[Spawner[Any]].isAssignableFrom(cls)) {
      Some(cls.asInstanceOf[Class[Spawner[Any]]])
    } else {
      None
    }
    case _ => None
  }.toList

  def concreteSubClassList(
      spawnerCls: Class[_ <: Spawner[Any]]): List[Class[_ <: Spawner[Any]]] =
    spawnerClassList.filter(cls =>
      !isAnonymous(cls) &&
      !Modifier.isAbstract(cls.getModifiers()) &&
      !Modifier.isInterface(cls.getModifiers()))
}

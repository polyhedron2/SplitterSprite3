package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import java.lang.reflect.{Modifier}
import scala.reflect.{ClassTag}

import jp.gr.java_conf.polyhedron.splittersprite3.spawner.{Spawner}

abstract class ReflectionUtils {
  def classIterator: Iterator[Class[_]]

  def typeOf[T: ClassTag]: Class[T] =
    implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]

  // 無名クラスか否か
  // $Nで終わるクラス名は無名クラスに対応する
  def isAnonymous(cls: Class[_]): Boolean = try {
    val clsName = cls.toString().split("\\$", 0)
    val tailName = clsName(clsName.size - 1)
    tailName.toInt
    true
  } catch {
    case e: NumberFormatException => false
  }

  lazy val spawnerClassList: List[Class[_ <: Spawner[Any]]] =
    classIterator.flatMap {
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

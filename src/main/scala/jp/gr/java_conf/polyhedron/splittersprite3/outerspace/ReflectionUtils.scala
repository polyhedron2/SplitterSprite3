package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import java.lang.reflect.{Modifier, Method}
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

  def allDeclaredMethods(cls: Class[_]): Set[Method] =
    Set(cls.getDeclaredMethods():_*) ++
    Option(cls.getSuperclass()).map(allDeclaredMethods).getOrElse(Set()) ++
    cls.getInterfaces.flatMap(allDeclaredMethods)

  lazy val spawnerClassSet: Set[Class[_ <: Spawner[Any]]] =
    classIterator.flatMap { case cls =>
      if (classOf[Spawner[Any]].isAssignableFrom(cls)) {
        Some(cls.asInstanceOf[Class[Spawner[Any]]])
      } else {
        None
      }
    }.toSet

  def concreteSubClassSet(
      spawnerCls: Class[_ <: Spawner[Any]]): Set[Class[_ <: Spawner[Any]]] =
    spawnerClassSet.filter(cls =>
      spawnerCls.isAssignableFrom(cls) &&
      !isAnonymous(cls) &&
      !Modifier.isAbstract(cls.getModifiers()) &&
      !Modifier.isInterface(cls.getModifiers()))

  def callLazyVals(spawner: Spawner[Any]) {
    var called = Set[Spawner[Any]]()
    var calling = Set(spawner)

    while (!calling.isEmpty) {
      val target = calling.head
      val lazyValMethods = allDeclaredMethods(target.getClass()).filter(
        _.getName().endsWith("$lzycompute"))
      for (method <- lazyValMethods) {
        method.setAccessible(true)
        method.invoke(target) match {
          case next: Spawner[Any] => if (!called(next)) { calling += next }
          case _ =>
        }
      }

      calling -= target
      called += target
    }
  }
}

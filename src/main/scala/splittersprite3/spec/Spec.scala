package splittersprite3.spec

// Spiritから読み出された値の型を表現するクラス
sealed abstract class Spec
case class StringSpec(defaultOpt: Option[String]) extends Spec
case class BooleanSpec(defaultOpt: Option[Boolean]) extends Spec
case class IntSpec(defaultOpt: Option[Int]) extends Spec
case class DoubleSpec(defaultOpt: Option[Double]) extends Spec

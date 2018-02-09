package splittersprite3.spirit

class SpiritValueIsNotFound(path: String, field: String)
  extends Exception(s"${path}[${field}]の値が見つかりません。")
class SpiritValueIsInvalid(path: String, field: String, cause: Exception)
  extends Exception(s"${path}[${field}]の値が不正です。", cause)

// XMLファイルに実際に読み書きを実行する抽象クラス
abstract class RealSpirit extends Spirit {
  // XMLファイルアクセスロック用オブジェクト
  val lock: AnyRef

  // XMLファイル上に指定のfieldで文字列があればSomeでそれを返し、なければNone
  def rawValueOpt(field: String): Option[String]

  val stringOf = new RealAccessor[String] {
    def rawValue2Value(rawValue: String) = rawValue
    def value2RawValue(value: String) = value
  }

  val booleanOf = new RealAccessor[Boolean] {
    def rawValue2Value(rawValue: String) = rawValue.toBoolean
    def value2RawValue(value: Boolean) = value.toString
  }

  val intOf = new RealAccessor[Int] {
    def rawValue2Value(rawValue: String) = rawValue.toInt
    def value2RawValue(value: Int) = value.toString
  }

  val doubleOf = new RealAccessor[Double] {
    def rawValue2Value(rawValue: String) = rawValue.toDouble
    def value2RawValue(value: Double) = value.toString
  }

  abstract class RealAccessor[VALUE] extends Accessor[VALUE] {
    // XML上の文字列から指定のリテラルに変換
    def rawValue2Value(rawValue: String): VALUE
    // 指定のリテラルから文字列に変換
    def value2RawValue(value: VALUE): String

    override def apply(field: String) = lock.synchronized {
      try {
        rawValueOpt(field).map(rawValue2Value)
      } catch {
        // 文字列をリテラルに変換できなかった場合
        case e: Exception => throw new SpiritValueIsInvalid(path, field, e)
      }
    } getOrElse {
      // 文字列が設定されていなかった場合
      throw new SpiritValueIsNotFound(path, field)
    }

    override def apply(field: String, default: VALUE) = try {
      apply(field)
    } catch {
      // 文字列が設定されていなかった場合
      case e: SpiritValueIsNotFound => default
    }

    override def update(field: String, value: VALUE) {
      // TODO 設定値更新処理を実装
    }
  }
}

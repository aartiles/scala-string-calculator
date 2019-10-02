package string_calculator

object TokenType extends Enumeration {
  val Delimiter, Number, None = Value
}

final class Token(val value: String, val tokenType: TokenType.Value) {

}

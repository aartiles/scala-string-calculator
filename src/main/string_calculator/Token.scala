package string_calculator

object TokenType extends Enumeration {
  val Delimiter, Number, None = Value
}

object Token {
  def createNumber(value: String): Token = new Token(value, TokenType.Number)
  def createDelimiter(value: String): Token = new Token(value, TokenType.Delimiter)
  def createInvalid(value: String): Token = new Token(value, TokenType.None)
}

final class Token private (val value: String, val tokenType: TokenType.Value)

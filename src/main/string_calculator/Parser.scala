package string_calculator

object Parser {
  def create(input: String): Parser = {
    val delimiterRegExp = """^//(.+)\n""".r.unanchored

    input match {
      case delimiterRegExp(delimiter) => {
        new Parser(input.replaceAll(delimiterRegExp.toString, ""), delimiter)
      }
      case _ => {
        new Parser(input)
      }
    }
  }
}

final class Parser private (val input: String, val delimiter: String = null) {
  private val DELIMITERS = Array("\n", ",")
  private var position = 0

  def parse: Array[Double] = {
    var nextType = TokenType.Number
    var numbers = Array.empty[Double]

    do {
      val prevPosition = this.position
      val token = this.nextToken

      if (nextType == TokenType.Number && token.tokenType == TokenType.Number) {
        numbers = numbers :+ token.value.toDouble
        nextType = TokenType.Delimiter
      }
      else if (nextType == TokenType.Number && token.tokenType == TokenType.Delimiter) {
        throw new RuntimeException(s"Number expected but '${token.value}' found at position $prevPosition.")
      }
      else if (nextType == TokenType.Delimiter && token.tokenType == TokenType.Delimiter && this.isEOF) {
        throw new RuntimeException("Number expected but EOF found")
      }
      else {
        nextType = TokenType.Number
      }
    }
    while (!this.isEOF)
    numbers
  }

  private def nextToken: Token = {
    var token = ""
    var tokenType = TokenType.None

    do {
      val c = this.currentChar

      if (this.isNumeric(c) && (tokenType == TokenType.Number || tokenType == TokenType.None)) {
        token += c
        tokenType = TokenType.Number
        this.position += 1
        if (this.isEOF) return Token.createNumber(token)
      }
      else if (!this.isNumeric(c) && tokenType == TokenType.Number) {
        return Token.createNumber(token)
      }
      else if (!this.isNumeric(c) && (tokenType == TokenType.Delimiter || tokenType == TokenType.None)) {
        token += c
        tokenType = TokenType.Delimiter
        this.position += 1
        if (this.isDelimiter(token)) return  Token.createDelimiter(token)
      }
      else if (this.isNumeric(c) && tokenType == TokenType.Delimiter) {
        return Token.createInvalid(token)
      }
      else return Token.createInvalid(token)
    }
    while (!this.isEOF)
    return Token.createInvalid(token)
  }

  private def isDelimiter(token: String): Boolean = {
    if (token == this.delimiter)
      true
    else
      this.DELIMITERS.contains(token)
  }

  private def isNumeric(c: Char): Boolean = "0123456789.".contains(c)

  private def currentChar: Char = this.input.charAt(this.position)

  private def isEOF: Boolean = this.position >= this.input.length

}

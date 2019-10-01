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

final class Parser(val input: String, val delimiter: String = null) {
  private val DELIMITERS = Array("\n", ",")
  private var position = 0

  def parse: Array[Double] = {
    var nextType = "number"
    var numbers = Array.empty[Double]

    do {
      val prevPosition = this.position
      val (token, tokenType) = this.nextToken

      if (nextType == "number" && tokenType == "number") {
        numbers = numbers :+ token.toDouble
        nextType = "delimiter"
      }
      else if (nextType == "number" && tokenType == "delimiter") {
        throw new RuntimeException(s"Number expected but '$token' found at position $prevPosition.")
      }
      else if (nextType == "delimiter" && tokenType == "delimiter" && this.position > this.input.length - 1) {
        throw new RuntimeException("Number expected but EOF found")
      }
      else {
        nextType = "number"
      }

    }
    while (this.position < input.length)
    numbers
  }

  private def nextToken: (String, String) = {
    var token = ""
    var tokenType = ""

    do {
      val c = this.input.charAt(this.position)

      if (this.isNumeric(c) && (tokenType == "number" || tokenType.isEmpty)) {
        token += c
        tokenType = "number"
        this.position += 1
      }
      else if (!this.isNumeric(c) && tokenType == "number") {
        return (token, tokenType)
      }
      else if (!this.isNumeric(c) && (tokenType == "delimiter" || tokenType.isEmpty)) {
        token += c
        tokenType = "delimiter"
        this.position += 1
        if (this.isDelimiter(token)) return (token, tokenType)
      }
      else if (this.isNumeric(c) && tokenType == "delimiter") {
        return (token, "invalid")
      }
      else return (token, "invalid")

    }
    while (this.position < this.input.length)
    return (token, tokenType)
  }

  private def isDelimiter(token: String): Boolean = {
    if (token == this.delimiter)
      true
    else
      this.DELIMITERS.contains(token)
  }

  private def isNumeric(c: Char): Boolean = "0123456789.".contains(c)

}

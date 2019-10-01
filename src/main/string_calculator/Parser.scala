package string_calculator

object Parser {
  def create(input: String): Parser = {
    val delimiterRegExp = """^//(.+)\n""".r.unanchored
    input match {
      case delimiterRegExp(delimiter) => {
        println("delimiter", delimiter)
        println("new input", input.replaceAll(delimiterRegExp.toString, ""))
        new Parser(input.replaceAll(delimiterRegExp.toString, ""), delimiter)
      }
      case _ => {
        println("no match", input)
        new Parser(input)
      }
    }
  }
}

final class Parser(val input: String, val delimiter: String = null) {
  private val DELIMITERS = Array("\n", "","")
  private var position = 0

  def parse: Array[Double] = {
    var nextType = "number"
    var numbers = Array.empty[Double]

    do {
      val prevPosition = this.position
      val (token, tokenType) = this.nextToken
      println("token", token, tokenType, this.position)
      println("nextType before", nextType)

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
    println("nextType after", nextType)
    println("numbers size", numbers.length)
    numbers
  }

  private def nextToken: (String, String) = {
    var token = ""
    var tokenType = ""

    do {
      val c = this.input.charAt(this.position)
      if ((tokenType == "number" || tokenType.isEmpty) && this.isNumeric(c)) {
        token += c
        tokenType = "number"
      }
      else if (tokenType.isEmpty) {
        token += c
        tokenType = "delimiter"
      }
      else if (this.isDelimiter(token + c)) {
        token += c
        tokenType = "delimiter"
        return (token, tokenType)
      }
      else {
        return (token, tokenType)
      }
      this.position += 1
    }
    while (this.position < this.input.length)
    return (token, tokenType)
  }

  private def isDelimiter(token: String): Boolean = {
    if (token.toString == this.delimiter)
      true
    else
      this.DELIMITERS contains token
  }

  private def isNumeric(c: Char): Boolean = "0123456789.".contains(c)

}
